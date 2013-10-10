
structure LangCommon = 
struct

datatype var = string
datatype LR = Left | Right

datatype contEntry = Stage1 of type1 | Stage2 of type2
type 'a context = (var * 'a) list


end
