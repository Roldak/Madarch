--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"
with Glfw.Input.Keys;

package GLFW_Utils is
   -- abstraction layer for tests so they can run on both GLFW 2 and 3.
   -- only exposes most basic functionality, not intended for usage outside
   -- tests.

   procedure Init;

   procedure Open_Window
     (Width, Height : Natural; Title : String; Depth_Bits : Natural := 0);

   procedure Center_Cursor (DX, DY : out Float);

   procedure Swap_Buffers;

   procedure Poll_Events;

   procedure Wait_For_Events;

   procedure Set_Window_Title (Value : String);

   function Key_Pressed (K : GLfw.Input.Keys.Key) return Boolean;

   function Window_Opened return Boolean;

   procedure Close_Window;

   procedure Shutdown;

   procedure Configure_Minimum_OpenGL_Version (Major, Minor : Natural);

end GLFW_Utils;
