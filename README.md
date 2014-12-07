noise_erlang
============

perlin and simplex noise library for erlang

The functions have only been written, not even compiled and checked yet,
still testing

will update the readme once testing is complete

*remember erlang is 1 based, so cannpot start with 0,0 ,so when syncing with te javascript lib, offset the 0 base to 1 base


UPDATE:

application now compiles, not tested yet

UPDATE2:

tested:

perlin2
perlin3
simplex2
simplex3

sample gen_server calls:
gen_server:call({global,noise_erlang_gen},{perlin2, 0, 0}).
gen_server:call({global,noise_erlang_gen},{perlin2, 1, 1}).
gen_server:call({global,noise_erlang_gen},{perlin2, 1, 1.01}).
gen_server:call({global,noise_erlang_gen},{perlin2, 27, 27}).

gen_server:call({global,noise_erlang_gen},{perlin3, 0, 0, 0}).
gen_server:call({global,noise_erlang_gen},{perlin3, 1, 1, 1}).
gen_server:call({global,noise_erlang_gen},{perlin3, 1, 1, 1.01}).
gen_server:call({global,noise_erlang_gen},{perlin3, 27, 27, 1.01}).

gen_server:call({global,noise_erlang_gen},{simplex2, 0, 0}).
gen_server:call({global,noise_erlang_gen},{simplex2, 1, 1}).
gen_server:call({global,noise_erlang_gen},{simplex2, 1, 1.01}).
gen_server:call({global,noise_erlang_gen},{simplex2, 27, 27}).

gen_server:call({global,noise_erlang_gen},{simplex3, 0, 0, 0}).
gen_server:call({global,noise_erlang_gen},{simplex3, 1, 1, 1}).
gen_server:call({global,noise_erlang_gen},{simplex3, 1, 1, 1.01}).
gen_server:call({global,noise_erlang_gen},{simplex3, 27, 27, 1.01}).
