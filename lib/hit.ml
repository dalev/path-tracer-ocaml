open Base

type t = {shader_space: Shader_space.t; emit: Color.t; do_scatter: float -> Scatter.t}

let scatter t u = t.do_scatter u
let emit t = t.emit
let shader_space t = t.shader_space
