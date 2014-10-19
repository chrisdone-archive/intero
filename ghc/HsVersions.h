/* Hack needed because of http://hackage.haskell.org/trac/ghc/ticket/8040 */

#define ASSERT(e)      if debugIsOn && not (e) then (assertPanic __FILE__ __LINE__) else

#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = Util.global (value);
