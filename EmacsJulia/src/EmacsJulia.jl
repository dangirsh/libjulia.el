module EmacsJulia

export clean_sexpr

@noinline function linefilter!(expr::Expr)
    total = length(expr.args)
    i = 0
    while i < total
        i += 1
        if expr.args[i] |> typeof == Expr
            if expr.args[i].head == :line
                deleteat!(expr.args,i)
                total -= 1
                i -= 1
            else
                expr.args[i] = linefilter!(expr.args[i])
            end
        elseif expr.args[i] |> typeof == LineNumberNode
            deleteat!(expr.args,i)
            total -= 1
            i -= 1
        end
    end
    return expr
end

clean_sexpr_impl(ex) = sprint(io -> show(io, ex))

function clean_sexpr_impl(ex::Expr)
    head = clean_sexpr_impl(ex.head)
    args = join([clean_sexpr_impl(arg) for arg = ex.args], " ")
    "($head $args)"
end

function clean_sexpr(ex::Expr)
    ex = linefilter!(ex)
    clean_sexpr_impl(ex)
end

function clean_sexpr(julia_code::String)
    ex = Meta.parse(julia_code)
    ex = linefilter!(ex)
    clean_sexpr_impl(ex)
end


end
