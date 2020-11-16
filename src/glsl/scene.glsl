const float max_dist = 20;

// sphere

struct Sphere {
   vec3 center;
   float radius;
   int material_id;
};

float dist_to_sphere(vec3 x, Sphere s) {
   return length(s.center - x) - s.radius;
}

vec3 sphere_normal(vec3 x, Sphere s) {
   return normalize(x - s.center);
}

// plane

struct Plane {
   vec3 normal;
   float offset;
   int material_id;
};

float dist_to_plane(vec3 x, Plane p) {
   return dot(x, p.normal) + p.offset;
}

vec3 plane_normal(vec3 x, Plane p) {
   return p.normal;
}

// cube

struct Cube {
   vec3 center;
   float side;
   int material_id;
};

float dist_to_cube(vec3 x, Cube c) {
   vec3 q = abs(c.center - x) - vec3(c.side);
   return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}

vec3 cube_normal(vec3 x, Cube c) {
   vec3 d = x - c.center;
   float rx = abs(d.x);
   float ry = abs(d.y);
   float rz = abs(d.z);
   return normalize(vec3(
      float(rx >= ry - epsilon) * float(rx >= rz - epsilon) * sign(d.x),
      float(ry >= rx - epsilon) * float(ry >= rz - epsilon) * sign(d.y),
      float(rz >= rx - epsilon) * float(rz >= ry - epsilon) * sign(d.z)
   ));
}

// point light

struct PointLight {
   vec3 position;
   vec3 color;
};

vec3 sample_point_light(PointLight l, vec3 pos, vec3 normal,
                        out vec3 dir, out float dist) {
   dir = l.position - pos;
   dist = length(dir);
   dir /= dist;

   float attenuation = 1.0 / (dist * dist * 0.03);

   return l.color * min(attenuation, 1.5);
}

// spot light

struct SpotLight {
   vec3 position;
   vec3 direction;
   float aperture;
   vec3 color;
};

vec3 sample_spot_light(SpotLight l, vec3 pos, vec3 normal,
                       out vec3 dir, out float dist) {
   dir = l.position - pos;
   dist = length(dir);
   dir /= dist;

   float attenuation = 1.0 / (dist * dist * 0.03);
   float cos_theta = max(dot(-dir, l.direction), 0.0);
   float theta = acos(cos_theta);
   float ratio = clamp(theta / l.aperture, 0.0, 1.0);
   float visible = 1.0 - pow(ratio, 8.0);

   return l.color * min(attenuation, 1.5) * visible;
}

// scene description

layout(std140, binding = 1) uniform scene_description {
   int prim_sphere_count;
   Sphere prim_spheres[M_MAX_SPHERE_COUNT];

   int prim_plane_count;
   Plane prim_planes[M_MAX_PLANE_COUNT];

   int prim_cube_count;
   Cube prim_cubes[M_MAX_CUBE_COUNT];

   int point_light_count;
   PointLight point_lights[M_MAX_POINT_LIGHT_COUNT];

   int spot_light_count;
   SpotLight spot_lights[M_MAX_SPOT_LIGHT_COUNT];

   int light_count;
};

layout(binding = 2) uniform sampler3D custom_sdf;
layout(std140, binding = 3) uniform custom_description {
   vec3 custom_position;
   vec3 custom_extent;
};

float dist_to_custom(vec3 x) {
   vec3 q = x - custom_position;
   vec3 in_bounds = clamp(q, -custom_extent, custom_extent);
   vec3 q_to_bounds = in_bounds - q;
   float squared_dist_to_bounds = dot(q_to_bounds, q_to_bounds);
   if (squared_dist_to_bounds > epsilon) {
      return sqrt(squared_dist_to_bounds);
   }
   vec3 rebased = ((in_bounds / custom_extent) + vec3(1)) * 0.5;
   float dist_to_shell = texture(custom_sdf, rebased).r;
   return dist_to_shell;
}

vec3 custom_normal(vec3 x) {
   const float h = 0.002; // replace by an appropriate value
   const vec2 k = vec2(1,-1);
   return normalize(k.xyy * dist_to_custom(x + k.xyy * h) +
                    k.yyx * dist_to_custom(x + k.yyx * h) +
                    k.yxy * dist_to_custom(x + k.yxy * h) +
                    k.xxx * dist_to_custom(x + k.xxx * h));
}

float closest_primitive(vec3 x) {
   float closest = max_dist;
   for (int i = 0; i < prim_sphere_count; ++i) {
      closest = min(closest, dist_to_sphere (x, prim_spheres[i]));
   }
   for (int i = 0; i < prim_plane_count; ++i) {
      closest = min(closest, dist_to_plane (x, prim_planes[i]));
   }
   for (int i = 0; i < prim_cube_count; ++i) {
      closest = min(closest, dist_to_cube (x, prim_cubes[i]));
   }
   closest = min(closest, dist_to_custom (x));
   return closest;
}

float closest_primitive_info(vec3 x, out int index) {
   float closest = max_dist;
   for (int i = 0; i < prim_sphere_count; ++i) {
      float dist = dist_to_sphere (x, prim_spheres[i]);
      if (dist < closest) {
         closest = dist;
         index = i;
      }
   }
   for (int i = 0; i < prim_plane_count; ++i) {
      float dist = dist_to_plane (x, prim_planes[i]);
      if (dist < closest) {
         closest = dist;
         index = M_MAX_SPHERE_COUNT + i;
      }
   }
   for (int i = 0; i < prim_cube_count; ++i) {
      float dist = dist_to_cube (x, prim_cubes[i]);
      if (dist < closest) {
         closest = dist;
         index = M_MAX_SPHERE_COUNT + M_MAX_PLANE_COUNT + i;
      }
   }
   float dist = dist_to_custom (x);
   if (dist < closest) {
      closest = dist;
      index = M_MAX_SPHERE_COUNT + M_MAX_PLANE_COUNT + M_MAX_CUBE_COUNT;
   }
   return closest;
}

void primitive_info(int index, vec3 pos, out vec3 normal, out int material_id) {
   if (index < M_MAX_SPHERE_COUNT) {
      normal = sphere_normal(pos, prim_spheres[index]);
      material_id = prim_spheres[index].material_id;
      return;
   }
   index -= M_MAX_SPHERE_COUNT;
   if (index < M_MAX_PLANE_COUNT) {
      normal = plane_normal(pos, prim_planes[index]);
      material_id = prim_planes[index].material_id;
      return;
   }
   index -= M_MAX_PLANE_COUNT;
   if (index < M_MAX_CUBE_COUNT) {
      normal = cube_normal(pos, prim_cubes[index]);
      material_id = prim_cubes[index].material_id;
      return;
   }
   normal = custom_normal (pos);
   material_id = 3;
}

vec3 sample_light(int index, vec3 pos, vec3 normal, out vec3 dir, out float dist) {
   if (index < point_light_count) {
      return sample_point_light(point_lights[index], pos, normal, dir, dist);
   }
   index -= point_light_count;
   if (index < spot_light_count) {
      return sample_spot_light(spot_lights[index], pos, normal, dir, dist);
   }
}

