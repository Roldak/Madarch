#version 120
#define SPHERE 0
#define PLANE 1
#define CUBE 2

const float epsilon = 0.001f;
const float min_step_size = 0.005f;
const float max_dist = 20;
const int max_steps = 300;

varying vec4 pos;

//sphere

float dist_to_sphere(vec3 x, vec3 center, float radius) {
   return length(center - x) - radius;
}

vec3 sphere_normal(vec3 x, vec3 center, float radius) {
   return normalize(x - center);
}

//plane

float dist_to_plane(vec3 x, vec3 normal, float height) {
   return dot(x, normal) + height;
}

vec3 plane_normal(vec3 x, vec3 normal, float height) {
   return normal;
}

//cube

float dist_to_cube(vec3 x, vec3 center, float size) {
   vec3 q = abs(center - x) - vec3(size);
   return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

vec3 cube_normal(vec3 x, vec3 center, float size) {
   vec3 d = x - center;
   float rx = abs(d.x);
   float ry = abs(d.y);
   float rz = abs(d.z);
   return normalize(vec3(
      float(rx >= ry - epsilon) * float(rx >= rz - epsilon) * sign(d.x),
      float(ry >= rx - epsilon) * float(ry >= rz - epsilon) * sign(d.y),
      float(rz >= rx - epsilon) * float(rz >= ry - epsilon) * sign(d.z)
   ));
}

uniform float time;
uniform vec3 camera_position;

struct Primitive {
   int kind;
   vec3 vec3_param_1;
   float float_param_1;

   vec3 color;
   float mirror;
};

float dist_to_primitive(vec3 x, Primitive prim) {
   switch (prim.kind) {
      case SPHERE:
         return dist_to_sphere(x, prim.vec3_param_1, prim.float_param_1);
      case PLANE:
         return dist_to_plane(x, prim.vec3_param_1, prim.float_param_1);
      case CUBE:
         return dist_to_cube(x, prim.vec3_param_1, prim.float_param_1);
   }
}

vec3 primitive_normal(vec3 x, Primitive prim) {
   switch (prim.kind) {
      case SPHERE:
         return sphere_normal(x, prim.vec3_param_1, prim.float_param_1);
      case PLANE:
         return plane_normal(x, prim.vec3_param_1, prim.float_param_1);
      case CUBE:
         return cube_normal(x, prim.vec3_param_1, prim.float_param_1);
   }
}

#define PRIM_COUNT 8
const Primitive prims[PRIM_COUNT] = Primitive[](
   Primitive(SPHERE, vec3(0.5, 0.5, 1.0), 1.0, vec3(1, 0, 0), 0),
   Primitive(CUBE,   vec3(-0.5, 0, 2.0),  0.8, vec3(0, 0, 1), 0),

   Primitive(PLANE,  vec3(0, 1, 0),   1.0, vec3(0.5, 0.5, 0.5), 0),
   Primitive(PLANE,  vec3(0, -1, 0), 5.0, vec3(0.5, 0.5, 0.5), 0),
   Primitive(PLANE,  vec3(1, 0, 0),  5.0, vec3(0.5, 0.5, 0.5), 0),
   Primitive(PLANE,  vec3(-1, 0, 0),  5.0, vec3(0.5, 0.5, 0.5), 0),
   Primitive(PLANE,  vec3(0, 0, 1),  5.0, vec3(0.5, 0.5, 0.5), 0),
   Primitive(PLANE,  vec3(0, 0, -1),  5.0, vec3(0.5, 0.5, 0.5), 0)
);

float closest_primitive(vec3 x, out int index) {
   float closest = max_dist;
   for (int i = 0; i < PRIM_COUNT; ++i) {
      float dist = dist_to_primitive (x, prims[i]);
      if (dist < closest) {
         closest = dist;
         index = i;
      }
   }
   return closest;
}

bool raycast(vec3 from, vec3 dir, out int index, out vec3 coll, out float closest) {
   float total_dist = 0;

   closest = max_dist;

   for (int i = 0; i < max_steps; ++i) {
      float dist = closest_primitive(from, index);

      if (dist < epsilon) {
         coll = from + dir * dist;
         closest = 0;
         return true;
      }

      closest = min(closest, dist);
      float step = max(dist, min_step_size);
      from += dir * step;
      total_dist += step;

      if (total_dist > max_dist) {
         return false;
      }

   }
   return false;
}

vec3 shade(vec3 pos, vec3 color, vec3 normal, vec3 light_pos) {
   // shadow cast
   int dummy_index;
   vec3 occluder_pos;
   vec3 shadow_ray_dir = normalize(light_pos - pos);
   vec3 shadow_ray_pos = pos + normal * min_step_size * 5;
   float closest;

   raycast(shadow_ray_pos, shadow_ray_dir, dummy_index, occluder_pos, closest);

   float shadows = max((0.1 - closest) * 10, 0);

   // lambert
   vec3 light_dir = normalize(light_pos - pos);
   float NoL = max(dot(normal, light_dir), 0.0);
   vec3 LDirectional = vec3(0.9, 0.9, 0.8) * NoL;
   vec3 LAmbient = vec3(0.03, 0.04, 0.1);
   vec3 diffuse = color * (LDirectional + LAmbient);

   return mix(diffuse, diffuse * 0.1, shadows);
}

vec3 fog(vec3 from, vec3 pos, vec3 col, vec3 bg) {
   float dist = length(pos - from);
   float offset = dist - 0.8 * max_dist;
   return mix(col, bg, max(offset / (0.2 * max_dist), 0));
}

vec3 pixel_color_0(vec3 from, vec3 dir, vec3 light_pos) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   float closest_dist;
   if (raycast(from, dir, prim_index, pos, closest_dist)) {
      vec3 color = prims[prim_index].color;
      vec3 normal = primitive_normal(pos, prims[prim_index]);
      vec3 result = shade(pos, color, normal, light_pos);
      return fog(from, pos, result, background_color);

   }
   return background_color;
}

vec3 pixel_color_1(vec3 from, vec3 dir, vec3 light_pos) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   float closest_dist;
   if (raycast(from, dir, prim_index, pos, closest_dist)) {
      vec3 color = prims[prim_index].color;
      vec3 normal = primitive_normal(pos, prims[prim_index]);
      vec3 result = shade(pos, color, normal, light_pos);

      // reflection

      float mirror = prims[prim_index].mirror;
      if (mirror > 0) {
         result = mix(
            result,
            pixel_color_0(
               pos + normal * min_step_size * 5,
               reflect(dir, normal),
               light_pos
            ),
            mirror
         );
      }

      return fog(from, pos, result, background_color);
   }
   return background_color;
}

vec3 pixel_color_2(vec3 from, vec3 dir, vec3 light_pos) {
   vec3 background_color = vec3(0.30, 0.36, 0.60) - (dir.y * 0.7);

   int prim_index;
   vec3 pos;
   float closest_dist;
   if (raycast(from, dir, prim_index, pos, closest_dist)) {
      vec3 color = prims[prim_index].color;
      vec3 normal = primitive_normal(pos, prims[prim_index]);
      vec3 result = shade(pos, color, normal, light_pos);

      // reflection

      float mirror = prims[prim_index].mirror;
      if (mirror > 0) {
         result = mix(
            result,
            pixel_color_1(
               pos + normal * min_step_size * 5,
               reflect(dir, normal),
               light_pos
            ),
            mirror
         );
      }

      return fog(from, pos, result, background_color);
   }
   return background_color;
}

void main(void)
{
   vec3 frag_pos = vec3(pos.xy, 0);
   vec3 dir = normalize(frag_pos - vec3(0, 0, -1.5));
   vec3 light_pos = vec3(cos(time), 2, sin(time));
   vec3 initial_pos = frag_pos + camera_position;

   gl_FragColor = vec4(
      pow(pixel_color_1(initial_pos, dir, light_pos), vec3(0.4545)),
      1
   );
}
