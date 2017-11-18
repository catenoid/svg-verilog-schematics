import logging
logging.basicConfig(level=logging.WARNING)

# read svg
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('infile',
    help="specifiy a file")
args = parser.parse_args()

# create dom
from xml.dom import minidom
from svg.path import parse_path
doc = minidom.parse(args.infile)

# extract paths (these will be connectors + (unhelpfully) the arrowheads on those connectors
paths = doc.getElementsByTagName("path")

class Connector:
  def __init__(self, start, end, width, ident):
    self.width = width
    self.ident = ident
    # these are complex numbers x + y*i
    # The getters are .real, .imag
    self.start = start
    self.end = end
  def __str__(self):
    return "Wire "+self.ident

links = [] # keep track of all connectors

class WireException(Exception):
    pass

class WireWidthUnspecified(WireException):
    """The Description box in Inkscape must contain an integer = the width of the wire"""
    pass

class WireDirectionUnspecified(WireException):
    """At least one end of the connector must have an arrowhead"""
    pass

class AmbiguousWireDirection(WireException):
    """A wire can't point in both directions (until inout ports...)"""
    pass

wire_index = 0
for path in paths:
    p = parse_path(path.getAttribute("d"))
    if (not p.closed):
        # Discount the arrowheads, which are closed

        # extract the width, specified in the box
        desc = path.getElementsByTagName("desc")
        if len(desc) > 0:
            width = int(desc[0].childNodes[0].nodeValue)
        else:
            raise WireWidthUnspecified

        ident = 'wire'+str(wire_index)
        wire_index += 1

        # Directionality of connector?
        end1, end2 = p[0].start, p[-1].end

        has_front_arrowhead = "marker-end" in path.getAttribute("style")
        has_back_arrowhead = "marker-start" in path.getAttribute("style")
        if (has_front_arrowhead and has_back_arrowhead):
            raise AmbiguousWireDirection            
        elif (has_front_arrowhead):
            links.append(Connector(end1, end2, width, ident))
        elif (has_back_arrowhead):
            links.append(Connector(end2, end1, width, ident))
        else:
            raise WireDirectionUnspecified


class Box:
  def __init__(self, c, w, h, body):
    self.c = c
    self.w = w
    self.h = h
    self.body = body

    self.inputs = [] # tuples of width, ident?
    self.outputs = []

  def __str__(self):
    return "Box " + str(int(self.center.real)) + " " + str(int(self.center.imag))

  def __repr__(self):
    return str(self)

  def __eq__(self, other):
    if not isinstance(other, Box):
      return False
    # eventually this will depend on what module the instance is of
    return self.c == other.c and self.w == other.w and self.h == other.h

  def __hash__(self):
    return int(self.c.real + self.c.imag)

  def distanceTo(self, c2):
    c1 = self.center
    return (c1.real-c2.real)**2 + (c1.imag-c2.imag)**2

  @property
  def center(self):
    return complex(self.c.real + (self.w/2), self.c.imag + (self.h/2))


groups = doc.getElementsByTagName("g")

boxes = []

for g in groups:
    if (g.getAttribute("id") == u'layer1'):
        # a group is defined for the top level layer
        continue  

    # there needs to be a way for a box to store its functionality
    body = {}
    title = g.getElementsByTagName("title")
    if len(title) > 0:
        title_value = title[0].childNodes[0].nodeValue
        body['title'] = title_value
        
    desc = g.getElementsByTagName("desc")
    if len(desc) > 0:
        desc_value = desc[0].childNodes[0].nodeValue
        body['desc'] = desc_value

    rect = g.getElementsByTagName("rect")[0]
    cornerx = rect.getAttribute("x")
    cornery = rect.getAttribute("y")
    corner = complex(float(cornerx), float(cornery))
    height = float(rect.getAttribute("height"))
    width = float(rect.getAttribute("width"))

    box = Box(corner, width, height, body)
    boxes.append(box)


# identify which connectors interface to which boxes
def getClosestBox(boxes, endpoint):
    if len(boxes) > 1:
        closest = boxes[0]
        smallestSeparation = closest.distanceTo(endpoint)
        for box in boxes[1:]:
            d = box.distanceTo(endpoint)
            if d < smallestSeparation:
                smallestSeparation = d
                closest = box
        return closest


def build_wire_decl(width, ident):
    if (width == 1):
        return "wire "+ident+";"
    else:
        return "wire ["+str(width-1)+":0] "+ident+";"


import networkx as nx
G = nx.MultiDiGraph()

logging.info("Printing wire declarations")


class TestBench:
    """Collect what's required for the simulation in a named tuple"""
    def __init__(self):
        self.module_decls = ""
        self.wire_decls = ""
        self.module_instances = ""

tb = TestBench()

for i, link in enumerate(links):
    outof = getClosestBox(boxes, link.start)
    into = getClosestBox(boxes, link.end)
    G.add_edge(outof, into, connector=link)
    tb.wire_decls += build_wire_decl(link.width, link.ident)


for node in G.nodes():
    logging.info("Data for node: %s" % node)
    nInputs = G.in_degree(node)
    nOutputs = G.out_degree(node)
    logging.info("   "+str(nInputs)+" inputs")
    logging.info("   "+str(nOutputs)+" outputs")

    # annotate the boxes with the nodes
    for u,v,d in G.out_edges(node, data=True):
        c = d['connector']
        logging.info("wire out to "+str(v)+" width "+str(c.width)+" called "+c.ident)
        node.outputs.append(c.ident)

    for u,v,d in G.in_edges(node, data=True):
        c = d['connector']
        logging.info("wire in from "+str(u)+" width "+str(c.width)+" called "+c.ident)
        node.inputs.append(c.ident)

    logging.info("")

# DECLARATIONS
# ------------
timescale = """`timescale 1ns / 1ns"""

# source module, depends on how many successor Boxes it has
source_template = """
module source (count, {output_idents});
input [7:0] count;
{output_decls}
{assign_from_count}
endmodule
"""


def build_decl(is_input, width, ident):
    return ("in" if is_input else "out")+"put ["+str(width-1)+":0] "+ident+";"


def to_csv(idents):
    if len(idents) == 0:
        return ""
    elif len(idents) == 1:
        return idents[0]
    else:
        return idents[0] + ", " + to_csv(idents[1:])


def build_source_assigns(idents):
    return '\n'.join("assign %s = count;" % ident for ident in idents)


def build_source_decl(n_outputs):
    idents = ['out'+str(i) for i in range(n_outputs)]
    return source_template.format(**{'output_idents': to_csv(idents), \
                                     'output_decls': '\n'.join([ \
                                         build_decl(False, 8, ident) \
                                         for ident in idents ]), \
                                     'assign_from_count': build_source_assigns(idents)
                                    })



# this will eventually look at the width of the input, but always returns one bit
comparator_template = """
module comparator (in0, out0);
input [7:0] in0;
output out0;
assign out0 = (in0 > 5) ? 1 : 0;
endmodule"""


tb.module_decls += comparator_template


class ModuleException(Exception):
    pass


class ImproperPortSpec(ModuleException):
    pass


class SourceInstance:
    def __init__(self, wires_in, wires_out):
        if (len(wires_in) != 0):
            raise ImproperPortSpec
        self.idents = wires_out

    def __str__(self):
        source_instantiation = """source source0 (count, {output_idents});"""
        return (source_instantiation.format(**{'output_idents': to_csv(self.idents)}))

# This depends on the size of wires_out in SourceInstance
tb.module_decls += build_source_decl(1)

class ComparatorInstance:
    def __init__(self, wires_in, wires_out):
        if (len(wires_in) != 1 or len(wires_out) != 1):
            raise ImproperPortSpec
        self.wire_in = wires_in[0]
        self.wire_out = wires_out[0]

    def __str__(self):
        comparator_instatiation = """comparator comp0 ({input}, {output});"""
        return (comparator_instatiation.format(**{'input': self.wire_in, \
                                                  'output': self.wire_out }))

# INSTATIATIONS
# -------------
# Use the idents as defined by the connector labelling

# Back when we were cheating:
#print(SourceInstance([], ['wire0']))
#print(ComparatorInstance(['wire0'], ['wire1']))


# every node in the graph is now annotated with the connectors that go in and out
# print the declarations 

title_to_module = { 'comparator' : ComparatorInstance,
                    'source' : SourceInstance }

for box in G.nodes():
    module_type = box.body['title']
    if (module_type in title_to_module):
        instance = title_to_module[module_type]
        tb.module_instances += str(instance(box.inputs, box.outputs))

testbench_template = """
`timescale 1ns / 1ns

{module_decls}

module test_counter;

reg clk, reset;
wire [7:0] count;

{wire_decls}

counter dut (count, clk, reset);

{module_instances}

initial // Clock generator
  begin
    clk = 0;
    forever #10 clk = !clk;
  end
  
initial	// Test stimulus
  begin
    reset = 0;
    #5 reset = 1;
    #4 reset = 0;
  end
  
initial
    $monitor($stime,, reset,, clk,,, count); 
    
endmodule    
"""

print(testbench_template.format(**{'module_decls': tb.module_decls,
                                 'wire_decls' : tb.wire_decls,
                                 'module_instances' : tb.module_instances
                                }))
