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

let exp_movement = (e: zexp, d: dir): option(zexp) =>
  switch (e, d) {
  | (Cursor(Var(_)), Child(_)) => None
  | (Cursor(Lam(x, e)), Child(One)) => Some(Lam(x, Cursor(e)))
  | (Cursor(Lam(_)), Child(Two)) => None
  | (Cursor(Ap(e1, e2)), Child(One)) => Some(LAp(Cursor(e1), e2))
  | (Cursor(Ap(e1, e2)), Child(Two)) => Some(RAp(e1, Cursor(e2)))
  | (Cursor(Num(_)), Child(_)) => None
  | (Cursor(Plus(e1, e2)), Child(One)) => Some(LPlus(Cursor(e1), e2))
  | (Cursor(Plus(e1, e2)), Child(Two)) => Some(RPlus(e1, Cursor(e2)))
  | (Cursor(Asc(e, t)), Child(One)) => Some(LAsc(Cursor(e), t))
  | (Cursor(Asc(e, t)), Child(Two)) => Some(RAsc(e, Cursor(t)))
  | (Cursor(EHole), Child(_)) => None
  | (Cursor(NEHole(e)), Child(One)) => Some(NEHole(Cursor(e)))
  | (Cursor(NEHole(_)), Child(Two)) => None

  | (Cursor(_), Parent) => None

  | (Lam(_), Child(_))
  | (LAp(_), Child(_))
  | (RAp(_), Child(_))
  | (LPlus(_), Child(_))
  | (RPlus(_), Child(_))
  | (LAsc(_), Child(_))
  | (RAsc(_), Child(_))
  | (NEHole(_), Child(_)) => None

  | (Lam(x, Cursor(e)), Parent) => Some(Cursor(Lam(x, e)))
  | (LAp(Cursor(e1), e2), Parent)
  | (RAp(e1, Cursor(e2)), Parent) => Some(Cursor(Ap(e1, e2)))
  | (LPlus(Cursor(e1), e2), Parent)
  | (RPlus(e1, Cursor(e2)), Parent) => Some(Cursor(Plus(e1, e2)))
  | (LAsc(Cursor(e), t), Parent)
  | (RAsc(e, Cursor(t)), Parent) => Some(Cursor(Asc(e, t)))
  | (NEHole(Cursor(e)), Parent) => Some(Cursor(NEHole(e)))

  | (
      Lam(
        _,
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
      ),
      Parent,
    )
  | (
      LAp(
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
        _,
      ),
      Parent,
    )
  | (
      RAp(
        _,
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
      ),
      Parent,
    )
  | (
      LPlus(
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
        _,
      ),
      Parent,
    )
  | (
      RPlus(
        _,
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
      ),
      Parent,
    )
  | (
      LAsc(
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
        _,
      ),
      Parent,
    )
  | (RAsc(_, LArrow(_) | RArrow(_)), Parent)
  | (
      NEHole(
        Lam(_) | LAp(_) | RAp(_) | LPlus(_) | RPlus(_) | LAsc(_) | RAsc(_) |
        NEHole(_),
      ),
      Parent,
    ) =>
    None
  };

let rec typ_action = (t: ztyp, a: action): ztyp => ();

let rec syn_action =
        (ctx: typctx, (e: zexp, t: htyp), a: action): (zexp, htyp) =>
  ()

and ana_action = (ctx: typctx, e: zexp, a: action, t: htyp): zexp => ();
