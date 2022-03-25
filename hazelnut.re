// Maybe Monad
let ( let* ) = (x: option('a), f: 'a => option('b)): option('b) =>
  switch (x) {
  | Some(x) => f(x)
  | None => None
  };

// Maybe Monad
let (let+) = (x: option('a), f: 'a => 'b): option('b) => {
  let* x = x;
  Some(f(x));
};

type htyp =
  | Arrow(htyp, htyp)
  | Num
  | Hole;

type hexp =
  | Var(string)
  | Lam(string, hexp)
  | Ap(hexp, hexp)
  | Num(int)
  | Plus(hexp, hexp)
  | Asc(hexp, htyp)
  | EHole
  | NEHole(hexp);

type ztyp =
  | Cursor(htyp)
  | LArrow(ztyp, htyp)
  | RArrow(htyp, ztyp);

type zexp =
  | Cursor(hexp)
  | Lam(string, zexp)
  | LAp(zexp, hexp)
  | RAp(hexp, zexp)
  | LPlus(zexp, hexp)
  | RPlus(hexp, zexp)
  | LAsc(zexp, htyp)
  | RAsc(hexp, ztyp)
  | NEHole(zexp);

type child =
  | One
  | Two;

type dir =
  | Child(child)
  | Parent;

type shape =
  | Arrow
  | Num
  | Asc
  | Var
  | Lam
  | Ap
  | Lit
  | Plus
  | NEHole;

type action =
  | Move(dir)
  | Construct(shape)
  | Del
  | Finish;

module TypCtx = Map.Make(String);
type typctx = TypCtx.t(htyp);

let rec erase_typ: ztyp => htyp =
  fun
  | Cursor(t) => t
  | LArrow(t1, t2) => Arrow(erase_typ(t1), t2)
  | RArrow(t1, t2) => Arrow(t1, erase_typ(t2));

let rec erase_exp: zexp => hexp =
  fun
  | Cursor(e) => e
  | Lam(x, e) => Lam(x, erase_exp(e))
  | LAp(e1, e2) => Ap(erase_exp(e1), e2)
  | RAp(e1, e2) => Ap(e1, erase_exp(e2))
  | LPlus(e1, e2) => Plus(erase_exp(e1), e2)
  | RPlus(e1, e2) => Plus(e1, erase_exp(e2))
  | LAsc(e, t) => Asc(erase_exp(e), t)
  | RAsc(e, t) => Asc(e, erase_typ(t))
  | NEHole(e) => NEHole(erase_exp(e));

let typ_movement = (e: ztyp, d: dir): option(ztyp) =>
  switch (e, d) {
  | (Cursor(Arrow(t1, t2)), Child(One)) => Some(LArrow(Cursor(t1), t2))
  | (Cursor(Arrow(t1, t2)), Child(Two)) => Some(RArrow(t1, Cursor(t2)))
  | (LArrow(Cursor(t1), t2), Parent)
  | (RArrow(t1, Cursor(t2)), Parent) => Some(Cursor(Arrow(t1, t2)))
  | _ => None
  };

let exp_movement = (e: zexp, d: dir): option(zexp) =>
  switch (e, d) {
  | (Cursor(Asc(e, t)), Child(One)) => Some(LAsc(Cursor(e), t))
  | (Cursor(Asc(e, t)), Child(Two)) => Some(RAsc(e, Cursor(t)))
  | (LAsc(Cursor(e), t), Parent)
  | (RAsc(e, Cursor(t)), Parent) => Some(Cursor(Asc(e, t)))

  | (Cursor(Lam(x, e)), Child(One)) => Some(Lam(x, Cursor(e)))
  | (Lam(x, Cursor(e)), Parent) => Some(Cursor(Lam(x, e)))

  | (Cursor(Plus(e1, e2)), Child(One)) => Some(LPlus(Cursor(e1), e2))
  | (Cursor(Plus(e1, e2)), Child(Two)) => Some(RPlus(e1, Cursor(e2)))
  | (LPlus(Cursor(e1), e2), Parent)
  | (RPlus(e1, Cursor(e2)), Parent) => Some(Cursor(Plus(e1, e2)))

  | (Cursor(Ap(e1, e2)), Child(One)) => Some(LAp(Cursor(e1), e2))
  | (Cursor(Ap(e1, e2)), Child(Two)) => Some(RAp(e1, Cursor(e2)))
  | (LAp(Cursor(e1), e2), Parent)
  | (RAp(e1, Cursor(e2)), Parent) => Some(Cursor(Ap(e1, e2)))

  | (Cursor(NEHole(e)), Child(One)) => Some(NEHole(Cursor(e)))
  | (NEHole(Cursor(e)), Parent) => Some(Cursor(NEHole(e)))

  | _ => None
  };

let rec typ_action = (t: ztyp, a: action): option(ztyp) =>
  switch (t, a) {
  | (t, Move(d)) => typ_movement(t, d)

  | (_, Del) => Some(Cursor(Hole))

  | (Cursor(t), Construct(Arrow)) => Some(RArrow(t, Cursor(Hole)))
  | (Cursor(Hole), Construct(Num)) => Some(Cursor(Num))

  | (LArrow(zt, ht), a) =>
    let+ zt' = typ_action(zt, a);
    LArrow(zt', ht);
  | (RArrow(ht, zt), a) =>
    let+ zt' = typ_action(zt, a);
    RArrow(ht, zt');

  | _ => None
  };

let rec syn_action =
        (ctx: typctx, (e: zexp, t: htyp), a: action): (zexp, htyp) =>
  ()

and ana_action = (ctx: typctx, e: zexp, a: action, t: htyp): zexp => ();
