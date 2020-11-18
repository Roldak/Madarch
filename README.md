# Madarch
Ray marching engine in Ada and GLSL.

## Features

- Real-time global illumination, based on the <a href=http://jcgt.org/published/0008/02/01/paper-lowres.pdf>DDGI</a> paper.
- Physically-based materials.
- Soft shadows.
- World-space reflections.
- Glossy reflections (*WIP*).
- World-space ambient occlusion.
- Arbitrary meshes (voxelization, *WIP*).

## Illustration

<p align="center">
  <img width=800 height=800 src="media/realtime_gi.gif"><br>
  <i>Figure 1</i>
</p>

- *Figure 1* showcases real-time global illumination: when the spot light targets the blue wall, only blue light bounces off it and propagate in the room, meaning everything appears blue. Similarly, in turns red as soon as the spot hits the red wall.
