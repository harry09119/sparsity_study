circuit Tile :
  module PE :
    input clock : Clock
    input reset : UInt<1>
    input io_inA : UInt<8>
    input io_inB : UInt<8>
    input io_inC : UInt<32>
    input io_inS : UInt<1>
    output io_outA : UInt<8>
    output io_outB : UInt<8>
    output io_outC : UInt<32>
    output io_outS : UInt<1>
    input io_reset : UInt<1>

    reg inp : UInt<8>, clock with :
      reset => (UInt<1>("h0"), inp) @[os_sa.scala 21:20]
    node _inp_T = mux(io_reset, UInt<1>("h0"), io_inA) @[os_sa.scala 22:13]
    reg wgt : UInt<8>, clock with :
      reset => (UInt<1>("h0"), wgt) @[os_sa.scala 26:20]
    node _wgt_T = mux(io_reset, UInt<1>("h0"), io_inB) @[os_sa.scala 27:13]
    node mul = mul(io_inA, io_inB) @[os_sa.scala 31:20]
    reg out0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), out0) @[os_sa.scala 34:21]
    reg out1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), out1) @[os_sa.scala 35:21]
    node _out0_T = add(out0, mul) @[os_sa.scala 37:47]
    node _out0_T_1 = tail(_out0_T, 1) @[os_sa.scala 37:47]
    node _out0_T_2 = mux(io_inS, _out0_T_1, io_inC) @[os_sa.scala 37:33]
    node _out0_T_3 = mux(io_reset, UInt<1>("h0"), _out0_T_2) @[os_sa.scala 37:14]
    node _out1_T = add(out1, mul) @[os_sa.scala 38:55]
    node _out1_T_1 = tail(_out1_T, 1) @[os_sa.scala 38:55]
    node _out1_T_2 = mux(io_inS, io_inC, _out1_T_1) @[os_sa.scala 38:33]
    node _out1_T_3 = mux(io_reset, UInt<1>("h0"), _out1_T_2) @[os_sa.scala 38:14]
    node _io_outC_T = mux(io_inS, out1, out0) @[os_sa.scala 39:17]
    reg switch : UInt<1>, clock with :
      reset => (UInt<1>("h0"), switch) @[os_sa.scala 41:23]
    node _switch_T = mux(io_reset, UInt<1>("h0"), io_inS) @[os_sa.scala 42:16]
    io_outA <= inp @[os_sa.scala 23:11]
    io_outB <= wgt @[os_sa.scala 28:11]
    io_outC <= _io_outC_T @[os_sa.scala 39:11]
    io_outS <= switch @[os_sa.scala 43:11]
    inp <= mux(reset, UInt<8>("h0"), _inp_T) @[os_sa.scala 21:20 os_sa.scala 21:20 os_sa.scala 22:7]
    wgt <= mux(reset, UInt<8>("h0"), _wgt_T) @[os_sa.scala 26:20 os_sa.scala 26:20 os_sa.scala 27:7]
    out0 <= mux(reset, UInt<32>("h0"), _out0_T_3) @[os_sa.scala 34:21 os_sa.scala 34:21 os_sa.scala 37:8]
    out1 <= mux(reset, UInt<32>("h0"), _out1_T_3) @[os_sa.scala 35:21 os_sa.scala 35:21 os_sa.scala 38:8]
    switch <= mux(reset, UInt<1>("h0"), _switch_T) @[os_sa.scala 41:23 os_sa.scala 41:23 os_sa.scala 42:10]

  module PErow :
    input clock : Clock
    input reset : UInt<1>
    input io_inA_0 : UInt<8>
    input io_inA_1 : UInt<8>
    input io_inA_2 : UInt<8>
    input io_inA_3 : UInt<8>
    input io_inB : UInt<8>
    input io_inC : UInt<32>
    output io_outA_0 : UInt<8>
    output io_outA_1 : UInt<8>
    output io_outA_2 : UInt<8>
    output io_outA_3 : UInt<8>
    output io_outB : UInt<8>
    output io_outC : UInt<32>
    input io_inS : UInt<1>
    input io_reset : UInt<1>

    inst perow_0 of PE @[os_sa.scala 61:36]
    inst perow_1 of PE @[os_sa.scala 61:36]
    inst perow_2 of PE @[os_sa.scala 61:36]
    inst perow_3 of PE @[os_sa.scala 61:36]
    io_outA_0 <= perow_0.io_outA @[os_sa.scala 67:16]
    io_outA_1 <= perow_1.io_outA @[os_sa.scala 67:16]
    io_outA_2 <= perow_2.io_outA @[os_sa.scala 67:16]
    io_outA_3 <= perow_3.io_outA @[os_sa.scala 67:16]
    io_outB <= perow_3.io_outB @[os_sa.scala 85:13]
    io_outC <= perow_0.io_outC @[os_sa.scala 86:13]
    perow_0.clock <= clock
    perow_0.reset <= reset
    perow_0.io_inA <= io_inA_0 @[os_sa.scala 66:21]
    perow_0.io_inB <= io_inB @[os_sa.scala 81:21]
    perow_0.io_inC <= perow_1.io_outC @[os_sa.scala 73:27]
    perow_0.io_inS <= io_inS @[os_sa.scala 83:21]
    perow_0.io_reset <= io_reset @[os_sa.scala 78:23]
    perow_1.clock <= clock
    perow_1.reset <= reset
    perow_1.io_inA <= io_inA_1 @[os_sa.scala 66:21]
    perow_1.io_inB <= perow_0.io_outB @[os_sa.scala 70:23]
    perow_1.io_inC <= perow_2.io_outC @[os_sa.scala 73:27]
    perow_1.io_inS <= perow_0.io_outS @[os_sa.scala 76:23]
    perow_1.io_reset <= io_reset @[os_sa.scala 78:23]
    perow_2.clock <= clock
    perow_2.reset <= reset
    perow_2.io_inA <= io_inA_2 @[os_sa.scala 66:21]
    perow_2.io_inB <= perow_1.io_outB @[os_sa.scala 70:23]
    perow_2.io_inC <= perow_3.io_outC @[os_sa.scala 73:27]
    perow_2.io_inS <= perow_1.io_outS @[os_sa.scala 76:23]
    perow_2.io_reset <= io_reset @[os_sa.scala 78:23]
    perow_3.clock <= clock
    perow_3.reset <= reset
    perow_3.io_inA <= io_inA_3 @[os_sa.scala 66:21]
    perow_3.io_inB <= perow_2.io_outB @[os_sa.scala 70:23]
    perow_3.io_inC <= io_inC @[os_sa.scala 82:26]
    perow_3.io_inS <= perow_2.io_outS @[os_sa.scala 76:23]
    perow_3.io_reset <= io_reset @[os_sa.scala 78:23]

  module Tile :
    input clock : Clock
    input reset : UInt<1>
    input io_inA_0 : UInt<8>
    input io_inA_1 : UInt<8>
    input io_inA_2 : UInt<8>
    input io_inA_3 : UInt<8>
    input io_inB_0 : UInt<8>
    input io_inB_1 : UInt<8>
    input io_inB_2 : UInt<8>
    input io_inB_3 : UInt<8>
    input io_inC_0 : UInt<32>
    input io_inC_1 : UInt<32>
    input io_inC_2 : UInt<32>
    input io_inC_3 : UInt<32>
    input io_inS_0 : UInt<1>
    input io_inS_1 : UInt<1>
    input io_inS_2 : UInt<1>
    input io_inS_3 : UInt<1>
    output io_outC_0 : UInt<32>
    output io_outC_1 : UInt<32>
    output io_outC_2 : UInt<32>
    output io_outC_3 : UInt<32>
    input io_reset : UInt<1>

    inst tile_0 of PErow @[os_sa.scala 101:35]
    inst tile_1 of PErow @[os_sa.scala 101:35]
    inst tile_2 of PErow @[os_sa.scala 101:35]
    inst tile_3 of PErow @[os_sa.scala 101:35]
    io_outC_0 <= tile_0.io_outC @[os_sa.scala 119:16]
    io_outC_1 <= tile_1.io_outC @[os_sa.scala 119:16]
    io_outC_2 <= tile_2.io_outC @[os_sa.scala 119:16]
    io_outC_3 <= tile_3.io_outC @[os_sa.scala 119:16]
    tile_0.clock <= clock
    tile_0.reset <= reset
    tile_0.io_inA_0 <= io_inA_0 @[os_sa.scala 108:27]
    tile_0.io_inA_1 <= io_inA_1 @[os_sa.scala 108:27]
    tile_0.io_inA_2 <= io_inA_2 @[os_sa.scala 108:27]
    tile_0.io_inA_3 <= io_inA_3 @[os_sa.scala 108:27]
    tile_0.io_inB <= io_inB_0 @[os_sa.scala 111:20]
    tile_0.io_inC <= io_inC_0 @[os_sa.scala 113:20]
    tile_0.io_inS <= io_inS_0 @[os_sa.scala 115:20]
    tile_0.io_reset <= io_reset @[os_sa.scala 117:22]
    tile_1.clock <= clock
    tile_1.reset <= reset
    tile_1.io_inA_0 <= tile_0.io_outA_0 @[os_sa.scala 106:27]
    tile_1.io_inA_1 <= tile_0.io_outA_1 @[os_sa.scala 106:27]
    tile_1.io_inA_2 <= tile_0.io_outA_2 @[os_sa.scala 106:27]
    tile_1.io_inA_3 <= tile_0.io_outA_3 @[os_sa.scala 106:27]
    tile_1.io_inB <= io_inB_1 @[os_sa.scala 111:20]
    tile_1.io_inC <= io_inC_1 @[os_sa.scala 113:20]
    tile_1.io_inS <= io_inS_1 @[os_sa.scala 115:20]
    tile_1.io_reset <= io_reset @[os_sa.scala 117:22]
    tile_2.clock <= clock
    tile_2.reset <= reset
    tile_2.io_inA_0 <= tile_1.io_outA_0 @[os_sa.scala 106:27]
    tile_2.io_inA_1 <= tile_1.io_outA_1 @[os_sa.scala 106:27]
    tile_2.io_inA_2 <= tile_1.io_outA_2 @[os_sa.scala 106:27]
    tile_2.io_inA_3 <= tile_1.io_outA_3 @[os_sa.scala 106:27]
    tile_2.io_inB <= io_inB_2 @[os_sa.scala 111:20]
    tile_2.io_inC <= io_inC_2 @[os_sa.scala 113:20]
    tile_2.io_inS <= io_inS_2 @[os_sa.scala 115:20]
    tile_2.io_reset <= io_reset @[os_sa.scala 117:22]
    tile_3.clock <= clock
    tile_3.reset <= reset
    tile_3.io_inA_0 <= tile_2.io_outA_0 @[os_sa.scala 106:27]
    tile_3.io_inA_1 <= tile_2.io_outA_1 @[os_sa.scala 106:27]
    tile_3.io_inA_2 <= tile_2.io_outA_2 @[os_sa.scala 106:27]
    tile_3.io_inA_3 <= tile_2.io_outA_3 @[os_sa.scala 106:27]
    tile_3.io_inB <= io_inB_3 @[os_sa.scala 111:20]
    tile_3.io_inC <= io_inC_3 @[os_sa.scala 113:20]
    tile_3.io_inS <= io_inS_3 @[os_sa.scala 115:20]
    tile_3.io_reset <= io_reset @[os_sa.scala 117:22]