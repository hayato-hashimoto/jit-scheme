(lambda x x)

(atom sexp1 sexp2 ...)
atom - *builtin

{{


((lambda (<1> <2>) (atom <1> a <1> (proc ... <1>) ...)) sexp1 sexp2 ... )

    call            call
   lambda           proc
<1>      <1>    <1>     <1>
<2>      <2>    <2>     <2>
         --     --      --
         <1>    <1>     <1>
          a      a       a
         <1>    <1>     <1>
                --    reducted
                ...
                <1>
