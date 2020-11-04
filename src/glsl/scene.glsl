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

layout(std140, binding = 1) uniform scene_description {
   int prim_sphere_count;
   Sphere prim_spheres[M_MAX_SPHERE_COUNT];

   int prim_plane_count;
   Plane prim_planes[M_MAX_PLANE_COUNT];

   int prim_cube_count;
   Cube prim_cubes[M_MAX_CUBE_COUNT];

   PointLight point_light;
};

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
   return closest;
}

void primitive_info(vec3 pos, int index, out vec3 normal, out int material_id) {
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
}

