;buildInfoPackage: chisel3, version: 3.4.1, scalaVersion: 2.12.12, sbtVersion: 1.3.10
circuit Tile : 
  module PE : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PErow : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>[4], flip inB : UInt<8>, flip inC : UInt<32>, outA : UInt<8>[4], outB : UInt<8>, outC : UInt<32>, flip inS : UInt<1>, flip reset : UInt<1>}
    
    inst perow_0 of PE @[os_sa.scala 61:36]
    perow_0.clock <= clock
    perow_0.reset <= reset
    inst perow_1 of PE_1 @[os_sa.scala 61:36]
    perow_1.clock <= clock
    perow_1.reset <= reset
    inst perow_2 of PE_2 @[os_sa.scala 61:36]
    perow_2.clock <= clock
    perow_2.reset <= reset
    inst perow_3 of PE_3 @[os_sa.scala 61:36]
    perow_3.clock <= clock
    perow_3.reset <= reset
    perow_0.io.inA <= io.inA[0] @[os_sa.scala 66:21]
    io.outA[0] <= perow_0.io.outA @[os_sa.scala 67:16]
    perow_0.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_1.io.inA <= io.inA[1] @[os_sa.scala 66:21]
    io.outA[1] <= perow_1.io.outA @[os_sa.scala 67:16]
    perow_1.io.inB <= perow_0.io.outB @[os_sa.scala 70:23]
    perow_0.io.inC <= perow_1.io.outC @[os_sa.scala 73:27]
    perow_1.io.inS <= perow_0.io.outS @[os_sa.scala 76:23]
    perow_1.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_2.io.inA <= io.inA[2] @[os_sa.scala 66:21]
    io.outA[2] <= perow_2.io.outA @[os_sa.scala 67:16]
    perow_2.io.inB <= perow_1.io.outB @[os_sa.scala 70:23]
    perow_1.io.inC <= perow_2.io.outC @[os_sa.scala 73:27]
    perow_2.io.inS <= perow_1.io.outS @[os_sa.scala 76:23]
    perow_2.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_3.io.inA <= io.inA[3] @[os_sa.scala 66:21]
    io.outA[3] <= perow_3.io.outA @[os_sa.scala 67:16]
    perow_3.io.inB <= perow_2.io.outB @[os_sa.scala 70:23]
    perow_2.io.inC <= perow_3.io.outC @[os_sa.scala 73:27]
    perow_3.io.inS <= perow_2.io.outS @[os_sa.scala 76:23]
    perow_3.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_0.io.inB <= io.inB @[os_sa.scala 81:21]
    perow_3.io.inC <= io.inC @[os_sa.scala 82:26]
    perow_0.io.inS <= io.inS @[os_sa.scala 83:21]
    io.outB <= perow_3.io.outB @[os_sa.scala 85:13]
    io.outC <= perow_0.io.outC @[os_sa.scala 86:13]
    
  module PE_4 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_5 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_6 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_7 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PErow_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>[4], flip inB : UInt<8>, flip inC : UInt<32>, outA : UInt<8>[4], outB : UInt<8>, outC : UInt<32>, flip inS : UInt<1>, flip reset : UInt<1>}
    
    inst perow_0 of PE_4 @[os_sa.scala 61:36]
    perow_0.clock <= clock
    perow_0.reset <= reset
    inst perow_1 of PE_5 @[os_sa.scala 61:36]
    perow_1.clock <= clock
    perow_1.reset <= reset
    inst perow_2 of PE_6 @[os_sa.scala 61:36]
    perow_2.clock <= clock
    perow_2.reset <= reset
    inst perow_3 of PE_7 @[os_sa.scala 61:36]
    perow_3.clock <= clock
    perow_3.reset <= reset
    perow_0.io.inA <= io.inA[0] @[os_sa.scala 66:21]
    io.outA[0] <= perow_0.io.outA @[os_sa.scala 67:16]
    perow_0.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_1.io.inA <= io.inA[1] @[os_sa.scala 66:21]
    io.outA[1] <= perow_1.io.outA @[os_sa.scala 67:16]
    perow_1.io.inB <= perow_0.io.outB @[os_sa.scala 70:23]
    perow_0.io.inC <= perow_1.io.outC @[os_sa.scala 73:27]
    perow_1.io.inS <= perow_0.io.outS @[os_sa.scala 76:23]
    perow_1.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_2.io.inA <= io.inA[2] @[os_sa.scala 66:21]
    io.outA[2] <= perow_2.io.outA @[os_sa.scala 67:16]
    perow_2.io.inB <= perow_1.io.outB @[os_sa.scala 70:23]
    perow_1.io.inC <= perow_2.io.outC @[os_sa.scala 73:27]
    perow_2.io.inS <= perow_1.io.outS @[os_sa.scala 76:23]
    perow_2.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_3.io.inA <= io.inA[3] @[os_sa.scala 66:21]
    io.outA[3] <= perow_3.io.outA @[os_sa.scala 67:16]
    perow_3.io.inB <= perow_2.io.outB @[os_sa.scala 70:23]
    perow_2.io.inC <= perow_3.io.outC @[os_sa.scala 73:27]
    perow_3.io.inS <= perow_2.io.outS @[os_sa.scala 76:23]
    perow_3.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_0.io.inB <= io.inB @[os_sa.scala 81:21]
    perow_3.io.inC <= io.inC @[os_sa.scala 82:26]
    perow_0.io.inS <= io.inS @[os_sa.scala 83:21]
    io.outB <= perow_3.io.outB @[os_sa.scala 85:13]
    io.outC <= perow_0.io.outC @[os_sa.scala 86:13]
    
  module PE_8 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_9 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_10 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_11 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PErow_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>[4], flip inB : UInt<8>, flip inC : UInt<32>, outA : UInt<8>[4], outB : UInt<8>, outC : UInt<32>, flip inS : UInt<1>, flip reset : UInt<1>}
    
    inst perow_0 of PE_8 @[os_sa.scala 61:36]
    perow_0.clock <= clock
    perow_0.reset <= reset
    inst perow_1 of PE_9 @[os_sa.scala 61:36]
    perow_1.clock <= clock
    perow_1.reset <= reset
    inst perow_2 of PE_10 @[os_sa.scala 61:36]
    perow_2.clock <= clock
    perow_2.reset <= reset
    inst perow_3 of PE_11 @[os_sa.scala 61:36]
    perow_3.clock <= clock
    perow_3.reset <= reset
    perow_0.io.inA <= io.inA[0] @[os_sa.scala 66:21]
    io.outA[0] <= perow_0.io.outA @[os_sa.scala 67:16]
    perow_0.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_1.io.inA <= io.inA[1] @[os_sa.scala 66:21]
    io.outA[1] <= perow_1.io.outA @[os_sa.scala 67:16]
    perow_1.io.inB <= perow_0.io.outB @[os_sa.scala 70:23]
    perow_0.io.inC <= perow_1.io.outC @[os_sa.scala 73:27]
    perow_1.io.inS <= perow_0.io.outS @[os_sa.scala 76:23]
    perow_1.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_2.io.inA <= io.inA[2] @[os_sa.scala 66:21]
    io.outA[2] <= perow_2.io.outA @[os_sa.scala 67:16]
    perow_2.io.inB <= perow_1.io.outB @[os_sa.scala 70:23]
    perow_1.io.inC <= perow_2.io.outC @[os_sa.scala 73:27]
    perow_2.io.inS <= perow_1.io.outS @[os_sa.scala 76:23]
    perow_2.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_3.io.inA <= io.inA[3] @[os_sa.scala 66:21]
    io.outA[3] <= perow_3.io.outA @[os_sa.scala 67:16]
    perow_3.io.inB <= perow_2.io.outB @[os_sa.scala 70:23]
    perow_2.io.inC <= perow_3.io.outC @[os_sa.scala 73:27]
    perow_3.io.inS <= perow_2.io.outS @[os_sa.scala 76:23]
    perow_3.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_0.io.inB <= io.inB @[os_sa.scala 81:21]
    perow_3.io.inC <= io.inC @[os_sa.scala 82:26]
    perow_0.io.inS <= io.inS @[os_sa.scala 83:21]
    io.outB <= perow_3.io.outB @[os_sa.scala 85:13]
    io.outC <= perow_0.io.outC @[os_sa.scala 86:13]
    
  module PE_12 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_13 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_14 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PE_15 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>, flip inB : UInt<8>, flip inC : UInt<32>, flip inS : UInt<1>, outA : UInt<8>, outB : UInt<8>, outC : UInt<32>, outS : UInt<1>, flip reset : UInt<1>}
    
    reg inp : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 21:20]
    node _inp_T = mux(io.reset, UInt<1>("h00"), io.inA) @[os_sa.scala 22:13]
    inp <= _inp_T @[os_sa.scala 22:7]
    io.outA <= inp @[os_sa.scala 23:11]
    reg wgt : UInt<8>, clock with : (reset => (reset, UInt<8>("h00"))) @[os_sa.scala 26:20]
    node _wgt_T = mux(io.reset, UInt<1>("h00"), io.inB) @[os_sa.scala 27:13]
    wgt <= _wgt_T @[os_sa.scala 27:7]
    io.outB <= wgt @[os_sa.scala 28:11]
    node mul = mul(io.inA, io.inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io.inS, _out0_T_1, io.inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io.reset, UInt<1>("h00"), _out0_T_2) @[os_sa.scala 37:14]
    out0 <= _out0_T_3 @[os_sa.scala 37:8]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io.inS, io.inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io.reset, UInt<1>("h00"), _out1_T_2) @[os_sa.scala 38:14]
    out1 <= _out1_T_3 @[os_sa.scala 38:8]
    node _io_outC_T = mux(io.inS, out1, out0) @[os_sa.scala 39:17]
    io.outC <= _io_outC_T @[os_sa.scala 39:11]
    reg switch : UInt<1>, clock with : (reset => (reset, UInt<1>("h00"))) @[os_sa.scala 41:23]
    node _switch_T = mux(io.reset, UInt<1>("h00"), io.inS) @[os_sa.scala 42:16]
    switch <= _switch_T @[os_sa.scala 42:10]
    io.outS <= switch @[os_sa.scala 43:11]
    
  module PErow_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inA : UInt<8>[4], flip inB : UInt<8>, flip inC : UInt<32>, outA : UInt<8>[4], outB : UInt<8>, outC : UInt<32>, flip inS : UInt<1>, flip reset : UInt<1>}
    
    inst perow_0 of PE_12 @[os_sa.scala 61:36]
    perow_0.clock <= clock
    perow_0.reset <= reset
    inst perow_1 of PE_13 @[os_sa.scala 61:36]
    perow_1.clock <= clock
    perow_1.reset <= reset
    inst perow_2 of PE_14 @[os_sa.scala 61:36]
    perow_2.clock <= clock
    perow_2.reset <= reset
    inst perow_3 of PE_15 @[os_sa.scala 61:36]
    perow_3.clock <= clock
    perow_3.reset <= reset
    perow_0.io.inA <= io.inA[0] @[os_sa.scala 66:21]
    io.outA[0] <= perow_0.io.outA @[os_sa.scala 67:16]
    perow_0.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_1.io.inA <= io.inA[1] @[os_sa.scala 66:21]
    io.outA[1] <= perow_1.io.outA @[os_sa.scala 67:16]
    perow_1.io.inB <= perow_0.io.outB @[os_sa.scala 70:23]
    perow_0.io.inC <= perow_1.io.outC @[os_sa.scala 73:27]
    perow_1.io.inS <= perow_0.io.outS @[os_sa.scala 76:23]
    perow_1.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_2.io.inA <= io.inA[2] @[os_sa.scala 66:21]
    io.outA[2] <= perow_2.io.outA @[os_sa.scala 67:16]
    perow_2.io.inB <= perow_1.io.outB @[os_sa.scala 70:23]
    perow_1.io.inC <= perow_2.io.outC @[os_sa.scala 73:27]
    perow_2.io.inS <= perow_1.io.outS @[os_sa.scala 76:23]
    perow_2.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_3.io.inA <= io.inA[3] @[os_sa.scala 66:21]
    io.outA[3] <= perow_3.io.outA @[os_sa.scala 67:16]
    perow_3.io.inB <= perow_2.io.outB @[os_sa.scala 70:23]
    perow_2.io.inC <= perow_3.io.outC @[os_sa.scala 73:27]
    perow_3.io.inS <= perow_2.io.outS @[os_sa.scala 76:23]
    perow_3.io.reset <= io.reset @[os_sa.scala 78:23]
    perow_0.io.inB <= io.inB @[os_sa.scala 81:21]
    perow_3.io.inC <= io.inC @[os_sa.scala 82:26]
    perow_0.io.inS <= io.inS @[os_sa.scala 83:21]
    io.outB <= perow_3.io.outB @[os_sa.scala 85:13]
    io.outC <= perow_0.io.outC @[os_sa.scala 86:13]
    
  module Tile : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip inA : UInt<8>[4], flip inB : UInt<8>[4], flip inC : UInt<32>[4], flip inS : UInt<1>[4], outC : UInt<32>[4], flip reset : UInt<1>}
    
    inst tile_0 of PErow @[os_sa.scala 101:35]
    tile_0.clock <= clock
    tile_0.reset <= reset
    inst tile_1 of PErow_1 @[os_sa.scala 101:35]
    tile_1.clock <= clock
    tile_1.reset <= reset
    inst tile_2 of PErow_2 @[os_sa.scala 101:35]
    tile_2.clock <= clock
    tile_2.reset <= reset
    inst tile_3 of PErow_3 @[os_sa.scala 101:35]
    tile_3.clock <= clock
    tile_3.reset <= reset
    tile_0.io.inA[0] <= io.inA[0] @[os_sa.scala 108:27]
    tile_0.io.inA[1] <= io.inA[1] @[os_sa.scala 108:27]
    tile_0.io.inA[2] <= io.inA[2] @[os_sa.scala 108:27]
    tile_0.io.inA[3] <= io.inA[3] @[os_sa.scala 108:27]
    tile_0.io.inB <= io.inB[0] @[os_sa.scala 111:20]
    tile_0.io.inC <= io.inC[0] @[os_sa.scala 113:20]
    tile_0.io.inS <= io.inS[0] @[os_sa.scala 115:20]
    tile_0.io.reset <= io.reset @[os_sa.scala 117:22]
    io.outC[0] <= tile_0.io.outC @[os_sa.scala 119:16]
    tile_1.io.inA[0] <= tile_0.io.outA[0] @[os_sa.scala 106:27]
    tile_1.io.inA[1] <= tile_0.io.outA[1] @[os_sa.scala 106:27]
    tile_1.io.inA[2] <= tile_0.io.outA[2] @[os_sa.scala 106:27]
    tile_1.io.inA[3] <= tile_0.io.outA[3] @[os_sa.scala 106:27]
    tile_1.io.inB <= io.inB[1] @[os_sa.scala 111:20]
    tile_1.io.inC <= io.inC[1] @[os_sa.scala 113:20]
    tile_1.io.inS <= io.inS[1] @[os_sa.scala 115:20]
    tile_1.io.reset <= io.reset @[os_sa.scala 117:22]
    io.outC[1] <= tile_1.io.outC @[os_sa.scala 119:16]
    tile_2.io.inA[0] <= tile_1.io.outA[0] @[os_sa.scala 106:27]
    tile_2.io.inA[1] <= tile_1.io.outA[1] @[os_sa.scala 106:27]
    tile_2.io.inA[2] <= tile_1.io.outA[2] @[os_sa.scala 106:27]
    tile_2.io.inA[3] <= tile_1.io.outA[3] @[os_sa.scala 106:27]
    tile_2.io.inB <= io.inB[2] @[os_sa.scala 111:20]
    tile_2.io.inC <= io.inC[2] @[os_sa.scala 113:20]
    tile_2.io.inS <= io.inS[2] @[os_sa.scala 115:20]
    tile_2.io.reset <= io.reset @[os_sa.scala 117:22]
    io.outC[2] <= tile_2.io.outC @[os_sa.scala 119:16]
    tile_3.io.inA[0] <= tile_2.io.outA[0] @[os_sa.scala 106:27]
    tile_3.io.inA[1] <= tile_2.io.outA[1] @[os_sa.scala 106:27]
    tile_3.io.inA[2] <= tile_2.io.outA[2] @[os_sa.scala 106:27]
    tile_3.io.inA[3] <= tile_2.io.outA[3] @[os_sa.scala 106:27]
    tile_3.io.inB <= io.inB[3] @[os_sa.scala 111:20]
    tile_3.io.inC <= io.inC[3] @[os_sa.scala 113:20]
    tile_3.io.inS <= io.inS[3] @[os_sa.scala 115:20]
    tile_3.io.reset <= io.reset @[os_sa.scala 117:22]
    io.outC[3] <= tile_3.io.outC @[os_sa.scala 119:16]
    
