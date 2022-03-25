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

type dir =
  | Child(int)
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

let rec typ_action = (t: ztyp, a: action): ztyp => ();

let rec syn_action =
        (ctx: typctx, (e: zexp, t: htyp), a: action): (zexp, htyp) =>
  ()

and ana_action = (ctx: typctx, e: zexp, a: action, t: htyp): zexp => ();
