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
