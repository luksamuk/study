(datatype expr
  
  X : number;
  ___________
  X : expr;

  X : symbol;
  ___________
  X : expr;

  X : symbol; Y : expr;
  =====================
  [lambda X Y] : expr;

  X : expr; Y : expr; Z : expr;
  =============================
  [if X Y Z] : expr;

  [lambda X Y] : expr; E : env;
  ================================
  [closure [lambda X Y] E] : expr;

  X : symbol; Y : expr; Z : expr;
  ===============================
  [let X Y Z] : expr;

  Op: sysop; X : expr; Y : expr;
  ===================
  [Op X Y] : expr;

  X : expr; Y : expr;
  ===================
  [X Y] : expr;)


(datatype sysop

  if (element? Op [+ / - * > < = <= >= =])
  ________________________________________
  Op : sysop;)


(datatype verified-types

  
  _________________________________________
  (symbol? Var) : verified >> Var : symbol;


  _____________________________________
  (number? N) : verified >> N : number;
  
  P : boolean;
  P : verified >> Q : A;
  (not P) : verified >> R : A;
  ____________________________
  (if P Q R) : A;

  P : verified, Q : verified >> R;
  ________________________________
  (and P Q) : verified >> R;

  L : (list A);
  ___________________________________
  (element? X L) : verified >> X : A;)


(datatype env

  ___________
  [] : env;

  Var : Symbol; Val : Expr; Env : env;
  ====================================
  [[Var | Val] | Env] : env;)




(define mini-lookup-env
  {symbol --> env --> expr}
  Var []                -> (error "Cannot find ~A in the environment!" Var)
  Var [[Var | Val] | _] -> Val
  Var [[_ | _] | Rest]  -> (mini-lookup-env Var Rest))


(define mini-handle-closure
  {expr --> expr --> expr}
  [closure [lambda Var Body] Env] Exp2 -> (mini-eval Body [[Var | (mini-eval Exp2 Env)] | Env])
  X _ -> (error "~A is not a closure!" X))


(define mini-eval
  {expr --> env --> expr}
  
  N Env               -> N where (number? N)
  
  [lambda X Y] Env    -> [closure [lambda X Y] Env]
  
  Var Env             -> (mini-eval (mini-lookup-env Var Env) Env)
                           where (symbol? Var)
  
  [Op M N] Env        -> (let IM (mini-eval M Env)
			      IN (mini-eval N Env)
			   (if (and (number? IM) (number? IN))
			       (Op IM IN)
			       (error "arithop applied to non-numeric argument")))
                           where (element? Op [+ - * /])
  
  [Op M N] Env        -> (let IM (mini-eval M Env)
			      IN (mini-eval N Env)
			   (if (and (number? IM) (number? IN))
			       (if (Op IM IN) 1 0)
			       (error "arithop applied to non-numeric argument")))
                           where (element? Op [= < > <= >=])
			       
  [let Var E1 E2] Env -> (mini-eval E2 [[Var | (mini-eval E1 Env)] | Env])
  
  [if Test ET EF] Env -> (let T (mini-eval Test Env)
			   (if (not (= T 0))
			       (mini-eval ET Env)
			       (mini-eval EF Env)))
  
  [Exp1 Exp2] Env     -> (let Cl (mini-eval Exp1 Env)
			   (mini-handle-closure Cl Exp2))
  
  Exp _               -> (error "Cannot understand expression ~A" Exp))

