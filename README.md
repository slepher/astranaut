# astranaut


## quote


### quick start
   
  with 
   
    -include_lib("astranaut/include/quote.hrl").

  you can use quote(Code) to represent ast of the code.
  
    qute(Code) | quote(Code, Line)
  
  the second param of varaibe is line number in ast
  
    quote(
      fun(_) ->
        ok
      end, 10).
      
  generates 
  
    {'fun', 10, {clauses, [{clause, 10, [{var, 10, '_'}], [], [{atom, 10, ok}]}]}}.

  if it is not presented, current line number of code is used
  
    10: quote(
    11:   fun(_) ->
    12:     ok
    13: end).
      
  generates ast of function which is
  
    {'fun' 10, {clauses, [{clause, 11, [{var, 11, '_'}], [], [{atom, 12, ok}]}]}}.
    
### variable binding

  multiple style variable binding is supported.
  
  V is one variable
    
    _X@V
    
  V is one expression
  
    unquote_xxx(V)
    
  meaning of X | xxx
  
  bind one ast, there is no X | xxx
  
    unquote(V) | _@V 

    V = {var, 10, 'Var'},
    quote({hello, World, unquote(V)}) =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, V]} =>
    {tuple, 1, [{atom, 1, hello}, {var, 1, 'World'}, {var, 10, 'Var'}]}
    
  bind a list of ast and join quoted ast, X = S, xxx = splicing.
  
    unquote_splicing(Vs) | _L@Vs 
   
    Vs = [{var, 2, 'Var'}, {atom, 2, atom}],
    quote({A, unquote_splicing(Vs), B}) => 
    {tuple, 1, [{var, 1, 'A'}, Vs ++ [{var, 1, 'B'}]]} =>
    {tuple, 1, [{var, 1, 'A'}, {var, 2, 'Var'}, {atom, 2, atom}, {var, 1, 'B'}]}
    
  bind atom, X = A, xxx = atom
    
    unquote_atom(A) | _A@A
    
    A = hello,
    quote(unquote_atom(A),0) =>
    {atom, 1, A} =>
    {atom, 1, hello}

  bind integer, X = I, xxx = integer

    unquote_integer(I) | _I@I
    
    B = 10,
    quote(unquote_integer(I), 0) =>
    {integer, 1, B} =>
    {integer, 1, 10}
    
  bind float, X = F, xxx = float
    
    unquote_float(F) | _F@F
    
    F = 1.3,
    quote(unquote_float(F)) => 
    {float, 0, F} =>
    {float, 0, 1.3}
    
  bind variable by name, X = V, xxx = var
  
    unquote_var(V) | _V@V
    
    V = 'Class'
    quote(X = unquote_var(V)) =>
    {match, 1, {var, 1, 'X'}, {var, 1, V} =>
    {match, 1, {var, 1, 'X'}, {var, 1, 'Class'}}
    
  bind term, X = T, xxx = term
    
    unquote_term(T) | _T@T

    T is any term like hello, 10, 1.3, {hello, world}, [hello, world], [{hello, world}, 10]
    T = [hello, {world, 0}]
    quote({foo, unquote_term(T)}) =>
    {tuple, [{atom, 1, foo}, astranaut:concrete(T, 1)}]} =>
    {tuple, [{atom, 1, foo}, {cons, 1, {atom, 1, hello},{cons, 1, {tuple, 1, [{atom, 1, world}, {integer, 1, 0}]}, {nil, 1}}}]}
    
  why two style of binding is needed,

  _X@V could be used in any part of quoted ast
  unquote_xxx(V) could not used in pattern ast
  
  it's legal:
  
    Class = 'Class0',
    Exception = 'Exception0',
    StackTrace = 'StackTrace0',
    quote(
      try
        throw(hello)
      catch
        _V@Class:_V@Exception:_V@StackTrace ->
          erlang:raise(_V@Class, _V@Exception, _V@StackTrace)
      end).
      
   it's illegal
   
    Class = 'Class0',
    Exception = 'Exception0',
    StackTrace = 'StackTrace0',
    quote(
      try
        A
      catch
        unquote_var(Class):unquote_var(Exception):unquote_var(StackTrace) ->
          erlang:raise(_V@Class, _V@Exception, _V@StackTrace)
      end).
      
   in other hand, V in unquote_xxx(V) could be any expression, it's more powerful than _X@V
   
### variable binding in pattern

   quote macro could also be used in pattern match such as 
   
   for limit of erlang ast format in pattern, some special forms is used
   
   left side of match
   
     quote(_A@Atom) = {atom, 1, A}
     
     =>
     
     {atom, _, Atom} = {atom, 1, A}
    
   function pattern
   
     macro_clause(quote = {hello, _A@World = World2} = C) ->
       quote({hello2, _A@World, _@World2,_@C});
     
     => 
     
     macro_clause({tuple, _, [{atom, _, hello}, {atom, _, World} = World2]} = C) ->
       {tuple, 2, {atom, 2, hello2}, {atom, 2, World}, World2, C}
       
   function call in function pattern
   
     macro_quote_call(#quote_call{module = _A@Module, function = hello, arguments = _@Arguments}) ->
       quote(_A@Module:hello2(_L@Arguments)). 
     
     =>
     
     macro_quote_call({call, _, {remote, _, {atom, _, Module}, {atom, _, hello}, Arguments}}) ->
       {call, 2, {remote, 2, {atom, 2, Module}, {atom, 2, hello2}}, Arguments}.
       
   case clause pattern:
   
     case Ast of
       quote(_A@Atom) ->
         Atom;
       _ ->
         other
     end.
     
     =>
     
     case ast of
         {atom, _, Atom} ->
             Atom;
         _ ->
             other
     end.
 

## astranaut_macro
