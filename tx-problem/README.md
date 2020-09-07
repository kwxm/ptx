# tx-problem
This has a couple of examples illustrating a problem I ran into.  Some datatypes and functions are defined in `Lib.hs`, but trying to convert them into PLC in `Main.hs` causes a plugin error when you try to run the program:

```
tx-problem-exe: Error: Unsupported feature: Kind: * -> TYPE ('GHC.Types.TupleRep '[ 'GHC.Types.LiftedRep])
Context: Compiling kind: *
                         -> TYPE ('GHC.Types.TupleRep '[ 'GHC.Types.LiftedRep])
```

`stack run` should reproduce this, although `stack.yaml` will have to
be modified to point to the correct Plutus directory.

I'm not sure whether this is a genuine problem or if I just shouldn't be trying
to do it.  