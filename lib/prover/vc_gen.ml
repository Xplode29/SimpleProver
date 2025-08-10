open Core.Ast
open Utils

let rec wp cmd post = match cmd with
  |Skip -> post, []
  |Assign (var, a) -> replace_formula var a post, []
  |Core.Ast.Seq (c1, c2) ->
    let wp1, vcs1 = wp c1 post in
    let wp2, vcs2 = wp c2 wp1 in
    wp2, vcs1 @ vcs2
  |If (b, c1, c2) ->
    let cond = form_of_bool b in
    let wp1, vcs1 = wp c1 post in
    let wp2, vcs2 = wp c2 post in
    let pre = AndF(ImplyF(cond, wp1), ImplyF(NotF cond, wp2)) in
    pre, vcs1 @ vcs2
  |While (b, inv, c) ->
    let cond = form_of_bool b in
    let wp_body, vcs_body = wp c inv in
    let vc_pres = ImplyF (AndF (inv, cond), wp_body) in
    let vc_end = ImplyF (AndF (inv, NotF cond), post) in
    inv, vc_pres :: vc_end :: vcs_body