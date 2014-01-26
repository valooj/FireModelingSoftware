module Types(Category(CAT),Choice(MAT,SBO,IFC),Mat(PairMat),Sbo(PairSbo),Ifc(ListISV)
,Paraml(PairLV),Idl(PairLD),Params(List_Parami_Cont,Vide),
Parami(Integer,List_id_params,Text),Cont(List_Cont,Vide1),
Value(PaintInt,Val),Event_room(PairRU),Undesired(PairInf,PairSup,And),Entry,
Idi,Text,Integer,Symbol,Room,Epsilon)


data Category = CAT Choice
data Choice = MAT String | SBO String | IFC String

data Mat = PairMat (Idl,Paraml)
data Sbo = PairSbo (Idl,Paraml)
data Ifc = ListISV [(Integer, Idi, Value)]

data Paraml = PairLV (String,Value)

data Idl = PairLD (String,Integer)

data Params = List_Parami_Cont (Parami,Cont) | Vide Epsilon
data Parami = Integer | List_id_params (Idi,Params) | Text
data Cont = List_Cont (Parami,Cont) | Vide1 Epsilon
data Value = PairInt (Integer, Integer) | Val String

data Event_room = PairRU (Room,Undesired)
data Undesired = PairInf (Symbol,Value) | PairSup (Symbol,Value) | And (Undesired,Undesired)

type Entry = Category
type Idi = String
type Text = String
type Integer = Int
type Symbol = String
type Room = String
type Epsilon = String

--module à détailler
--Ambiguous Integer