eval_bare <- function(expr, env) {
  .Call(export_eval_bare, expr, env)
}
