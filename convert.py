import logging
logging.basicConfig(level=logging.INFO)

# Read SVG as a command line arguement
import argparse
parser = argparse.ArgumentParser()
parser.add_argument('infile', help="specifiy a file")
args = parser.parse_args()

# Create DOM from parsing input SVG
from xml.dom import minidom
from svg.path import parse_path
doc = minidom.parse(args.infile)




# CONNECTORS / WIRES
# ------------------

# Extract all paths in the input
#     these will be the connectors between boxes and (unhelpfully) the arrowheads on those connectors
paths = doc.getElementsByTagName("path")

class Connector:
    """Object representing a connector between boxes"""
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
        # This test discount the arrowheads, which are closed paths

        # 1. Extract the wire width, specified in the SVG descriptor tag
        desc = path.getElementsByTagName("desc")
        if len(desc) > 0:
            width = int(desc[0].childNodes[0].nodeValue)
        else:
            raise WireWidthUnspecified

        # 2. Assign a unique identifier for each wire
        ident = 'wire'+str(wire_index)

        # 3. Establish the directionality of the connector
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

        logging.info("Add connector %s width %d" % (ident, width))
        wire_index += 1




# BOXES / MODULES
# ---------------

class Box:
    """Object representing a box in the schematic. For now, there is a 1-1 correspondence with Verilog modules"""
    def __init__(self, c, w, h, body):
        self.c = c
        self.w = w
        self.h = h
        self.body = body

        # tuples of (width, identifier string) for connecting wires
        self.inputs = []
        self.outputs = []

    def __str__(self):
        return "Box (" + str(int(self.center.real)) + " " + str(int(self.center.imag)) + ")"
  
    def __repr__(self):
        return str(self)
  
    def __eq__(self, other):
        if not isinstance(other, Box):
            return False
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

class BoxException(Exception):
    pass

class UndescribedBox(BoxException):
    """Every box needs a title, to explain its function in the schematic"""
    pass

for g in groups:
    if (g.getAttribute("id") == u'layer1'):
        # a group is defined for the top level layer, which we are ignoring (for now)
        continue  

    # 1. Collect the functionality description of a box from the text descriptions
    body = {}

    # The SVG 'title' is generically what the module does eg. compares
    title = g.getElementsByTagName("title")
    if len(title) > 0:
        title_value = title[0].childNodes[0].nodeValue
        body['title'] = title_value
    else:
        raise UndescribedBox
        
    # Some boxes have an additional descriptive string
    desc = g.getElementsByTagName("desc")
    if len(desc) > 0:
        desc_value = desc[0].childNodes[0].nodeValue
        body['desc'] = desc_value

    # 2. Collect the physical position description of the box
    rect = g.getElementsByTagName("rect")[0]
    cornerx = rect.getAttribute("x")
    cornery = rect.getAttribute("y")
    corner = complex(float(cornerx), float(cornery))
    height = float(rect.getAttribute("height"))
    width = float(rect.getAttribute("width"))

    box = Box(corner, width, height, body)
    boxes.append(box)
    logging.info("Added %s" % str(box))




# GRAPH / SCHEMATIC
# -----------------

# Identify which connectors interface to which boxes
import networkx as nx
G = nx.MultiDiGraph()

class TestBench:
    """Collect what's required for the simulation"""
    def __init__(self):
        self.module_decls = []
        self.wire_decls = []
        self.module_instances = []

tb = TestBench()

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


logging.info("Adding graph edges (wires)")
for i, link in enumerate(links):
    outof = getClosestBox(boxes, link.start)
    into = getClosestBox(boxes, link.end)
    logging.info("Adding edge from %s to %s" % (outof, into))
    G.add_edge(outof, into, connector=link)
    tb.wire_decls.append(build_wire_decl(link.width, link.ident))


for node in G.nodes():
    # Some debug info:
    logging.info("Data for node: %s" % node)
    nInputs = G.in_degree(node)
    nOutputs = G.out_degree(node)
    logging.info("   "+str(nInputs)+" inputs")
    logging.info("   "+str(nOutputs)+" outputs")

    # Annotate the boxes with which wires go into or out of them
    for u,v,d in G.out_edges(node, data=True):
        c = d['connector']
        logging.info("wire out to "+str(v)+" width "+str(c.width)+" called "+c.ident)
        node.outputs.append(c.ident)

    for u,v,d in G.in_edges(node, data=True):
        c = d['connector']
        logging.info("wire in from "+str(u)+" width "+str(c.width)+" called "+c.ident)
        node.inputs.append(c.ident)

    # Make the logger output easier to read
    logging.info("")





# WRITE VERILOG
# -------------

def build_decl(is_input, width, ident):
    return ("in" if is_input else "out")+"put ["+str(width-1)+":0] "+ident+";"


def to_csv(idents):
    if len(idents) == 0:
        return ""
    elif len(idents) == 1:
        return idents[0]
    else:
        return idents[0] + ", " + to_csv(idents[1:])


class TranspilingError(Exception):
    """Class of errors for anything occuring during construction of the Verilog source"""
    pass

class ImproperPortSpec(TranspilingError):
    """A module doesn't have the right number of input or output wires"""
    pass

class BinOpArguementsInsufficient(TranspilingError):
    pass

source_template = """
module source (count, {output_idents});
input [7:0] count;
{output_decls}
{assign_from_count}
endmodule
"""

class SourceInstance:
    def __init__(self, wires_in, wires_out):
        if (len(wires_in) != 0):
            raise ImproperPortSpec

        decl = self.build_source_decl(len(wires_out))
        tb.module_decls.append(decl)

        inst_templ = """source source0 (count, {output_idents});"""
        inst = inst_templ.format(**{'output_idents': to_csv(wires_out)})
        tb.module_instances.append(inst)

    def build_source_assigns(self, idents):
        return '\n'.join("assign %s = count;" % ident for ident in idents)
    
    def build_source_decl(self, n_outputs):
        idents = ['out'+str(i) for i in range(n_outputs)]
        assigns = self.build_source_assigns(idents)
        return source_template.format(**{'output_idents': to_csv(idents), \
                                         'output_decls': '\n'.join([ \
                                             build_decl(False, 8, ident) \
                                             for ident in idents ]), \
                                         'assign_from_count': assigns \
                                        })


logic_string_to_symbol = {'OR' : '|', 'AND' : '&'}

class LogicInstances:
    def __init__(self, tb):
        self.tb = tb

    def add_instance(self, desc, wires_in, wires_out):
        if len(wires_in) < 2:
            raise BinOpArguementsInsufficient
        
        verilog_symbol = logic_string_to_symbol[desc]
        operation = self.add_operators(wires_in, verilog_symbol)
        template = "assign {wire_out} = {operation};"
        assigns =  [template.format(**{'wire_out': wire_out, \
                                       'operation': operation}) \
                                       for wire_out in wires_out ]
        decls = '\n'.join(assigns)
        self.tb.module_instances.append(decls)

    def add_operators(self, idents, symbol):
        def interpolate(idents):
            if len(idents) == 1:
                return idents[0]
            else:
                return idents[0] + symbol + interpolate(idents[1:])
        return interpolate(idents)

comparator_template = """
module {module_name} (in0, out0);
input [7:0] in0;
output out0;
assign out0 = (in0 {op_string}) ? 1 : 0;
endmodule"""  # this will eventually look at the width of the input wire

class ComparatorInstances:
    def __init__(self, tb):
        self.tb = tb
        self.n_modules = 0

    def add_instance(self, desc_string, wires_in, wires_out):
        if (len(wires_in) != 1 or len(wires_out) != 1):
            raise ImproperPortSpec
        wire_in = wires_in[0]
        wire_out = wires_out[0]

        # op_string should be json without the surrounding curlies
        json_string = '{'+desc_string+'}'
        params = json.loads(json_string)
        op_string = params['op']+str(params['v'])
    
        # all modules have unique names for now
        module_name = 'comparator'+str(self.n_modules)
        self.n_modules += 1

        decl = comparator_template.format(**{'module_name': module_name, \
                                             'op_string' : op_string})
        self.tb.module_decls.append(decl)
        
        inst_templ = """{module_name} comp{n_modules} ({input}, {output});"""
        inst = inst_templ.format(**{'module_name': module_name, \
                                    'n_modules': self.n_modules, \
                                    'input': wire_in, \
                                    'output': wire_out })
        self.tb.module_instances.append(inst)


comparators = ComparatorInstances(tb)
comb_logic = LogicInstances(tb)

import json

for box in G.nodes():
    module_type = box.body['title']

    if (module_type == 'comparator'):
        op_string = box.body['desc']

        comparators.add_instance(op_string, box.inputs, box.outputs)

    elif (module_type == 'source'):
        SourceInstance(box.inputs, box.outputs)

    elif (module_type in logic_string_to_symbol):
        comb_logic.add_instance(module_type, box.inputs, box.outputs)

    elif (module_type == 'result'):
        tb.wire_decls.append("wire result;")

        if (len(box.inputs) != 1):
            raise ImproperPortSpec

        tb.module_instances.append("assign result = %s;" % box.inputs[0])

    else:
        logging.info("No constructor for title %s" % module_type)
        

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

print(testbench_template.format(**{'module_decls': '\n'.join(tb.module_decls),
                           'wire_decls' : '\n'.join(tb.wire_decls),
                           'module_instances' : '\n'.join(tb.module_instances)
                           }))
