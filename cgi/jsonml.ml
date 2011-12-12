open Json_type
open Json_type.Build

type attribute =
  | Id of string
  | Classes of string list
  | Src of string
  | Type of string
  | Href of string
  | Colspan of int
  | Other of (string * string)

type attl = attribute list
type col = attl

type element =
  | Table of attl * tablerow list
  | Div of attl * element list
  | Span of attl * element list
  | Text of string
  | Button of attl * string
  | Input of attl * string
  | Textarea of attl * string
  | A of attl * string
  | Ul of attl * element list list
  | Ol of attl * element list list
  | Img of attl
  | B of attl * string
  | Em of attl * string
  | P of attl * element list
  | Label of attl * element list
  | Br
and tablerow =
  | Tr of attl * tableelement list
  | Colgroup of col list
and tableelement =
  | Td of attl * element list
  | Th of attl * element list

let table ?(a = []) l = Table (a, l)
let div ?(a = []) l = Div (a, l)
let span ?(a = []) l = Span (a, l)
let text s = Text s
let button ?(a = []) s = Button (a, s)
let input ?(a = []) s = Input (a, s)
let textarea ?(a = []) l = Textarea (a, l)
let a ?(a = []) s = A (a, s)
let ul ?(a = []) els = Ul (a, els)
let ol ?(a = []) els = Ol (a, els)
let img a = Img a
let b ?(a = []) s = B (a, s)
let em ?(a = []) s = Em (a, s)
let p ?(a = []) l = P (a, l)
let label ?(a = []) l = Label (a, l)
let br = Br
let td ?(a = []) l = Td (a, l)
let th ?(a = []) l = Th (a, l)
let tr ?(a = []) l = Tr (a, l)
let colgroup l = Colgroup l

let json_of_atts atts =
  let att_to_object = function
    | Id s -> ("id", string s)
    | Classes sl -> ("class", string (String.concat " " sl))
    | Src s -> ("src", string s)
    | Type s -> ("type", string s)
    | Href s -> ("href", string s)
    | Colspan i -> ("colspan", string (string_of_int i))
    | Other(t,n) -> (t, string n)
  in
  objekt (List.map att_to_object atts)

let rec json_of_tablerows trl =
  let td_or_th = function
    | Td (a, l) -> array ((string "td")::(json_of_atts a)::(json_of_elements l))
    | Th (a, l) -> array ((string "th")::(json_of_atts a)::(json_of_elements l))
  in
  match trl with
    | (Colgroup cl)::t -> (array ((string "colgroup")::(List.map (fun c -> array [string "col"; json_of_atts c]) cl)))::(json_of_tablerows t)
    | (Tr (a, tel))::t -> (array ((string "tr")::(json_of_atts a)::(List.map td_or_th tel)))::(json_of_tablerows t)
    | [] -> []
and json_of_elements = function
  | (Table (a, l))::t    -> (array ((string "table")::(json_of_atts a)::(json_of_tablerows l)))::(json_of_elements t)
  | (Div (a, l))::t      -> (array ((string "div")::(json_of_atts a)::(json_of_elements l)))::(json_of_elements t)
  | (Span (a, l))::t     -> (array ((string "span")::(json_of_atts a)::(json_of_elements l)))::(json_of_elements t)
  | (Text s)::t          -> (string s)::(json_of_elements t)
  | (Button (a, s))::t   -> (array [string "button"; json_of_atts a; string s])::(json_of_elements t)
  | (Input (a, s))::t    -> (array [string "input"; json_of_atts a; string s])::(json_of_elements t)
  | (Textarea (a, s))::t -> (array [string "textarea"; json_of_atts a; string s])::(json_of_elements t)
  | (A (a, s))::t        -> (array [string "a"; json_of_atts a; string s])::(json_of_elements t)
  | (Ul (a, l))::t       -> (array ((string "ul")::(json_of_atts a)::(List.map (fun el -> array ((string "li")::(json_of_elements el))) l)))::(json_of_elements t)
  | (Ol (a, l))::t       -> (array ((string "ol")::(json_of_atts a)::(List.map (fun el -> array ((string "li")::(json_of_elements el))) l)))::(json_of_elements t)
  | (Img a)::t           -> (array [string "img"; json_of_atts a])::(json_of_elements t)
  | (B (a, s))::t        -> (array [string "b"; json_of_atts a; string s])::(json_of_elements t)
  | (Em (a, s))::t       -> (array [string "em"; json_of_atts a; string s])::(json_of_elements t)
  | (P (a, l))::t        -> (array ((string "p")::(json_of_atts a)::(json_of_elements l)))::(json_of_elements t)
  | (Label (a, l))::t    -> (array ((string "label")::(json_of_atts a)::(json_of_elements l)))::(json_of_elements t)
  | Br::t                -> (array [string "br"])::(json_of_elements t)
  | []                   -> []

let json_of_element el = List.hd (json_of_elements [el])
