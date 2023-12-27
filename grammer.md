$$
\begin{align}
    [\text{Prog}] &\to [\text{Stmt}]^* \\
    [\text{Stmt}] &\to
    \begin{cases}
        \text{exit}([\text{Expr}]); \\
        \text{let}\space\text{ident} = [\text{Expr}]; \\
        \text{if} ([\text{Expr}])[\text{Scope}] \text{else}[\text{if}\text{ or }\text{[Scope]}] \\
        [\text{Scope}] \\
        [\text{Reassign}]\\
        [\text{while([Expr])[Scope]}] \\
    \end{cases} \\
    \text{[Reassign]} &\to
    \begin{cases} 
    \text{ ident =[Expr]}  \\
    \text{ident /=[Expr]} \\
    \text{ident *=[Expr]} \\
    \text{ident += [Expr]} \\
    \text{ident -=[Expr]} \\

    \end{cases} \\
       \text{[Scope]} &\to \{[\text{Stmt}]^*\} \\
    [\text{Expr}] &\to
    \begin{cases}
        [\text{Term}] \\
        [\text{BinExpr}] \\
        [\text{BoolExpr}]
    \end{cases} \\
    [\text{BoolExpr}] &\to 
    \begin{cases} 
            \text{[Expr] and [Expr]} \\
            \text{[Expr] or [Expr]}\\
            \text{[Expr] == [Expr]}\\
            \text{[Expr] != [Expr]}\\ 
    \end{cases}\\
    [\text{BinExpr}] &\to
    \begin{cases}
        [\text{Expr}] * [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] / [\text{Expr}] & \text{prec} = 1 \\
        [\text{Expr}] + [\text{Expr}] & \text{prec} = 0 \\
        [\text{Expr}] - [\text{Expr}] & \text{prec} = 0 \\
    \end{cases} \\ 
    [\text{Term}] &\to
    \begin{cases}
        \text{int\_lit} \\
        \text{ident} \\
        ([\text{Expr}])
    \end{cases}
\end{align}
$$