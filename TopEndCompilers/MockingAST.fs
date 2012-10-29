namespace Mockingbird

module Util = 
    type erroring<'T> = Just of 'T | Error of string
    let (>>=) (e : erroring<'a>) (f : 'a -> erroring<'b>) : erroring<'b> = 
        match e with
        | Just(v) -> f v
        | Error(m) -> Error(m)
    let bind e f = e >>= f

module Environment = 
    open Util
    type env<'T> = list<string * 'T>

    let rec find (e : env<'T>) label = 
        match e with
        | (key, value)::_ when key = label -> Just(value)
        | (_,_) :: rest -> find rest label
        | [] -> Error("Could not find label " + label)

module Compiler = 
    open Util
    open Environment
    type setType = One | Many | Set
    type funcType = {input : setType; output : setType}
    type tupleType = {g_part : setType; s_part : setType}
    type mbSemiType = Untouched | Touched of funcType
    type mbType = mbSemiType * mbSemiType

    // mockingbird AST
    type branchPred = SSize | GSize | General of string
    type mbNode = Compose of mbNode * mbNode 
                | Branch of branchPred * mbNode * mbNode
                | Fix of string * mbType * mbNode
                | Call of string
                | Extern of string * mbType

    // annotated tree
    type annNode = ACompose of annNode * annNode * tupleType
                 | ABranch of branchPred * annNode * annNode
                 | AFix of string * annNode
                 | ACall of string
                 | AExtern of string

    // intermediate representation
    type irTupleType = GS of tupleType | Fragment
    type irFunctionType = irTupleType * irTupleType
    type irNode = IRCompose of irFunctionType * irNode * irNode
                | IRBranch of irFunctionType * branchPred * irNode * irNode
                | IRCall of irFunctionType * string    
    type irProgram = env<irNode>

    // helpers for annontateType function
    let composeFuncType f s =
        match f, s with 
        | Untouched, Untouched -> Just(Untouched)
        | Untouched, Touched(ts) -> Just(Touched(ts))
        | Touched(tf), Untouched -> Just(Touched(tf))
        | Touched(tf), Touched(ts) when tf.output = ts.input -> Just(Touched({input = tf.input; output = ts.output}))
        | Touched(_), Touched(_) -> Error("Type composition error.")

    let backCalcMiddle funcType concreteInput =
        match funcType with
        | Untouched -> concreteInput
        | Touched(tf) -> tf.output

    let coerceInputType t concreteInput = 
        match t with
        | Untouched -> Just({input = concreteInput; output = concreteInput})
        | Touched(tf) when tf.input = concreteInput -> Just(tf)
        | Touched(_)-> Error("Error coercing in a size branch.")

    let intersectType t1 t2 = 
        match t1, t2 with
        | Untouched, t2 -> Just(t2)
        | t1, Untouched -> Just(t1)
        | Touched(t1), Touched(t2) when t1=t2 -> Just(Touched(t1))
        | Touched(_), Touched(_) -> Error("Type intersection accross branch failed.")

    let forceTogether t1 t2 =
        match t1, t2 with
        | Set, _ -> Set
        | _, Set -> Set
        | Many, One -> Set
        | One, Many -> Set
        | One, One -> One
        | Many, Many -> Many

    // typecheck and annotate a mockingbird tree to make it an annotated tree (or error)
    let annotateType node = 
        let rec comp n (e : env<mbType>) = 
            match n with
            | Compose(first,second) -> 
                (comp first []) >>= fun (aFirst, (g1, s1)) ->
                (comp second e) >>= fun (aSecond, (g2, s2)) -> 
                (composeFuncType g1 g2) >>= fun tG -> 
                (composeFuncType s1 s2) >>= fun tS -> 
                let annotatedNodeConstructor g s = (
                    let midG = backCalcMiddle g1 g.input in
                    let midS = backCalcMiddle s1 s.input in
                    let firstNode = aFirst {input = g.input; output = midG} {input = s.input; output = midS} in
                    let secondNode = aSecond {input = midG; output = g.output} {input = midS; output = s.output} in
                    ACompose(firstNode, secondNode, {g_part = midG; s_part = midS})) in
                    Just(annotatedNodeConstructor, (tG,tS))
            | Branch(pred,left,right) -> 
                (comp left e) >>= fun (aLeft, (gL, sL)) ->
                (comp right e) >>= fun (aRight, (gR, sR)) -> 
                match pred with
                | GSize -> 
                    (coerceInputType gL One) >>= fun coercedL -> 
                    (coerceInputType gR Many) >>= fun coercedR -> 
                    (intersectType sL sR) >>= fun s -> 
                    Just((fun _ s -> ABranch(pred,(aLeft coercedL s),(aRight coercedR s))), (Touched {input = Many; output = forceTogether coercedL.output coercedR.output},s))
                | SSize -> 
                    (coerceInputType sL One) >>= fun coercedL -> 
                    (coerceInputType sR Many) >>= fun coercedR -> 
                    (intersectType gL gR) >>= fun g -> 
                    Just((fun g _ -> ABranch(pred,(aLeft g coercedL),(aRight g coercedR))), (g, Touched {input = Many; output = forceTogether coercedL.output coercedR.output}))
                | General(name) -> 
                    (intersectType gL gR) >>= fun g -> 
                    (intersectType sL sR) >>= fun s -> 
                    Just((fun g s -> ABranch(pred,(aLeft g s),(aRight g s))), (g,s))
            | Fix(label,label_type,child) -> 
                (comp child ((label,label_type)::e)) >>= fun (aChild, childType) ->
                Just((fun g s -> AFix(label,aChild g s)),childType)
            | Call(label) -> (find e label) >>= fun t -> Just((fun _ _ -> ACall(label)), t)
            | Extern(name, t) -> Just((fun _ _ -> AExtern(name)), t)
        in comp node []

    // doesn't have any renaming
    let serialize node topType = 
        let rec ser n (inType, outType) = 
            let t = GS(inType), GS(outType) in
            match n with
            | ACompose(first,second,midType) -> 
                let (prog1, node1) = ser first (inType,midType) in
                let (prog2, node2) = ser second (midType,outType) in
                List.append prog1 prog2, IRCompose(t, node1, node2)
            | ABranch(p,left,right) -> 
                match p with
                | GSize ->
                    let (prog1, node1) = ser left ({g_part = One; s_part = inType.s_part}, outType) in
                    let (prog2, node2) = ser right ({g_part = Many; s_part = inType.s_part}, outType) in
                    List.append prog1 prog2, IRBranch(t, p, node1, node2)
                | SSize ->
                    let (prog1, node1) = ser left ({g_part = inType.g_part; s_part = One}, outType) in
                    let (prog2, node2) = ser right ({g_part = inType.g_part; s_part = Many}, outType) in
                    List.append prog1 prog2, IRBranch(t, p, node1, node2)
                | General _ ->
                    let (prog1, node1) = ser left (inType, outType) in
                    let (prog2, node2) = ser right (inType, outType) in
                    List.append prog1 prog2, IRBranch(t, p, node1, node2)
            | AFix(label,child) -> 
                let (prog, node) = ser child (inType, outType) in
                ((label, node)::prog, IRCall(t,label))
            | ACall(label) -> [], IRCall(t,label)
            | AExtern(name) -> [], IRCall(t,name)
        in ser node topType

        
module ExampleRenderers =
    open Compiler
    let Node_1S = Extern("1S", (Untouched, Touched {input = Set; output = One}))
    let Node_2GP = Extern("2GP", (Touched {input = Many; output = Set}, Untouched))
    let Node_Id = Extern("Id", (Untouched, Untouched))
    let Type_ListNodes = Touched {input = Many; output = One}, Untouched
    let Standard_BVH = Compose(Node_1S, Fix("alpha", Type_ListNodes, Branch(GSize, Node_Id, Compose(Node_2GP, Call("alpha")))))
