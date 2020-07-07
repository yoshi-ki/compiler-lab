open KNormal

(* --- add for 2-2, Common subexpression elimination --- *)

(* judge if two knormal expressions are equal *)
let rec exp_eq e1 e2 =
  match (e1,e2) with
  | (Unit, Unit) -> true
  | (Int(a), Int(b)) -> if(a = b) then true else false
  | (Neg(a), Neg(b)) -> if(a = b) then true else false 
  | (Add(a,b),Add(c,d)) -> if((a=c) && (b=d)) then true else false
  | (Sub(a,b),Sub(c,d)) -> if((a=c) && (b=d)) then true else false
  | (FNeg(a), FNeg(b)) -> if(a = b) then true else false
  | (FAdd(a,b),FAdd(c,d)) -> if((a=c) && (b=d)) then true else false
  | (FSub(a,b),FSub(c,d)) -> if((a=c) && (b=d)) then true else false
  | (FMul(a,b),FMul(c,d)) -> if((a=c) && (b=d)) then true else false
  | (FDiv(a,b),FDiv(c,d)) -> if((a=c) && (b=d)) then true else false
  | (IfEq(a,b,c,d),IfEq(e,f,g,h)) -> if((a=e)&&(b=f)&&(exp_eq c g)&&(exp_eq d h)) then true else false
  | (IfLE(a,b,c,d),IfLE(e,f,g,h)) -> if((a=e)&&(b=f)&&(exp_eq c g)&&(exp_eq d h)) then true else false
  | (Var(a),Var(b)) -> if(a=b) then true else false
  | (App(x,xs),App(y,ys)) -> if((x=y) && (li_eq xs ys) ) then true else false
  | (Tuple(x),Tuple(y)) -> if(li_eq x y) then true else false
  | (Get(a,b),Get(c,d)) -> if((a=c) && (b=d)) then true else false
  | (Put(a,b,c),Put(d,e,f)) -> if((a=d) && (b=e) && (c=f)) then true else false
  | (ExtArray(a),ExtArray(b)) -> if (a=b) then true else false
  | _ -> false
and li_eq xs ys =
  match (xs,ys) with
  | ([],[]) -> true
  | ([],a) -> false
  | (a,[]) -> false
  | ((a::arest),(b::brest)) -> if(a=b) then (li_eq arest brest) else false


(*if expressions are equal, replace with var *)
let rec get_var e1 l =
  match l with 
  | [] -> e1
  | (x,e) :: rest -> if (exp_eq e e1) then (print_string "a"; print_kNormal e 0;print_kNormal e1 0; Var(x)) else get_var e1 rest


let rec elimination exp l =
  match exp with
  | IfEq(x, y, e1, e2) -> IfEq(x,y,(elimination e1 l), (elimination e2 l))
  | IfLE(x, y, e1, e2) -> IfLE(x,y,(elimination e1 l), (elimination e2 l))
  | Let((x, t), e1, e2) -> Let((x,t),(get_var e1 l),(let l2 = [(x,e1)] @ l in elimination e2 l2))
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> LetRec({ name = (x, t); args = yts; body = (elimination e1 l)},(elimination e2 l))
  | LetTuple(es,s,e1)-> LetTuple(es,s,elimination e1 l)
  | _ -> exp
(* --- add for 2.2 --- *)

(* インライン展開する関数の最大サイズ (caml2html: inline_threshold) *)
let threshold = ref 0 (* Mainで-inlineオプションによりセットされる *)

let rec size = function
  | IfEq(_, _, e1, e2) | IfLE(_, _, e1, e2)
  | Let(_, e1, e2) | LetRec({ body = e1 }, e2) -> 1 + size e1 + size e2
  | LetTuple(_, _, e) -> 1 + size e
  | _ -> 1

let rec g env = function (* インライン展開ルーチン本体 (caml2html: inline_g) *)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | Let(xt, e1, e2) -> Let(xt, g env e1, g env e2)
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* 関数定義の場合 (caml2html: inline_letrec) *)
      let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
      LetRec({ name = (x, t); args = yts; body = g env e1}, g env e2)
  | App(x, ys) when M.mem x env -> (* 関数適用の場合 (caml2html: inline_app) *)
      let (zs, e) = M.find x env in
      Format.eprintf "inlining %s@." x;
      let env' =
        List.fold_left2
          (fun env' (z, t) y -> M.add z y env')
          M.empty
          zs
          ys in
      Alpha.g env' e
  | LetTuple(xts, y, e) -> LetTuple(xts, y, g env e)
  | e -> e

let f e = 
        print_newline ();
  print_kNormal e 0;
  print_newline ();
  let e2 = elimination e [] in
  print_kNormal e2 0;
  g M.empty e2
