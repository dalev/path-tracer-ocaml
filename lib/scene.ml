open Base
module L = Low_discrepancy_sequence

type t = {bvh: Bvh.t; background: Ray.t -> Color.t; camera: Camera.t}

let create camera shapes ~background =
  let bvh =
    List.map shapes ~f:(fun s -> Shape.transform s ~f:(Camera.transform camera))
    |> Bvh.create in
  {bvh; background; camera}

let diffuse_plus_light_pdf (_ : t) =
  (* CR dalev: area lights *)
  Pdf.diffuse

let path_tracer t =
  let bvh = t.bvh in
  let background = t.background in
  let diffuse_plus_light = diffuse_plus_light_pdf t in
  let camera = t.camera in
  Staged.stage
  @@ fun ~cx ~cy max_bounces samples ->
  let ray = Camera.ray camera cx cy in
  let take_2d =
    let open L.Sample in
    let samples_index = ref 2 in
    fun () ->
      let j = !samples_index in
      let u = samples.%{j} and v = samples.%{j + 1} in
      samples_index := j + 2 ;
      (u, v) in
  let rec loop ray max_bounces =
    if max_bounces <= 0 then
      Color.black
    else
      let max_bounces = max_bounces - 1 in
      match Bvh.intersect bvh ray ~t_min:0.0 ~t_max:Float.max_finite_value with
      | None -> background ray
      | Some h -> (
          let emit = Hit.emit h in
          let u, v = take_2d () in
          match (Hit.scatter h u : Scatter.t) with
          | Absorb -> emit
          | Specular (scattered_ray, attenuation) ->
              let open Color.Infix in
              emit + (attenuation * loop scattered_ray max_bounces)
          | Diffuse attenuation ->
              let open Color.Infix in
              let ss = Hit.shader_space h in
              let dir = Pdf.sample diffuse_plus_light ss u v in
              let diffuse_pd = Pdf.eval Pdf.diffuse dir ss in
              if Float.( = ) diffuse_pd 0.0 then
                emit
              else
                let divisor = Pdf.eval diffuse_plus_light dir ss in
                let pd = diffuse_pd /. divisor in
                if not (Float.is_finite pd) then
                  emit
                else
                  let scattered_ray = Shader_space.world_ray ss dir in
                  emit + (Color.scale attenuation pd * loop scattered_ray max_bounces) )
  in
  loop ray max_bounces
