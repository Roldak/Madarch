--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Text_IO;

with Interfaces.C;

with Glfw.Windows.Context;
with Glfw.Windows.Hints;
with Glfw.Input.Keys;
with Glfw.Input.Mouse;
with Glfw.Errors;

package body GLFW_Utils is
   type Test_Window is new Glfw.Windows.Window with null record;

   overriding
   procedure Close_Requested (Object : not null access Test_Window) is
   begin
      Object.Destroy;
   end Close_Requested;

   type Main_Window_Type is not null access Test_Window;

   Main_Window : constant Main_Window_Type := new Test_Window;

   procedure Print_Error (Code : Glfw.Errors.Kind; Description : String) is
   begin
      Ada.Text_IO.Put_Line ("Error occured (" & Code'Img & "): " & Description);
   end Print_Error;

   procedure Enable_Print_Errors is
   begin
      Glfw.Errors.Set_Callback (Print_Error'Access);
   end Enable_Print_Errors;

   procedure Init is
   begin
      Enable_Print_Errors;
      Glfw.Init;
   end Init;

   procedure Open_Window
     (Width, Height : Natural; Title : String; Depth_Bits : Natural := 0)
   is
   begin
      if not Main_Window.Initialized then
         Glfw.Windows.Hints.Set_Depth_Bits (Depth_Bits);
         Main_Window.Init
           (Glfw.Size (Width), Glfw.Size (Height), Title);
      end if;
      Main_Window.Show;
      Main_Window.Enable_Callback (Glfw.Windows.Callbacks.Close);
      Glfw.Windows.Context.Make_Current (Main_Window);
   end Open_Window;

   procedure Center_Cursor (DX, DY : out Float) is
      use Interfaces.C;

      SX, SY : Int;
      MX, MY : Double;
   begin
      Main_Window.Get_Size (SX, SY);
      Main_Window.Get_Cursor_Pos (MX, MY);
      declare
         CX : Double := Double (SX) / 2.0;
         CY : Double := Double (SY) / 2.0;
      begin
         Main_Window.Set_Cursor_Pos (CX, CY);
         DX := Float (MX - CX);
         DY := Float (MY - CY);
      end;
   end Center_Cursor;

   procedure Show_Cursor (Show : Boolean) is
   begin
      if Show then
         Main_Window.Set_Cursor_Mode (Glfw.Input.Mouse.Normal);
      else
         Main_Window.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);
      end if;
   end Show_Cursor;

   procedure Swap_Buffers is
   begin
      Glfw.Windows.Context.Swap_Buffers (Main_Window);
   end Swap_Buffers;

   procedure Poll_Events is
   begin
      Glfw.Input.Poll_Events;
   end Poll_Events;

   procedure Wait_For_Events is
   begin
      Glfw.Input.Wait_For_Events;
   end Wait_For_Events;

   procedure Set_Window_Title (Value : String) is
   begin
      Main_Window.Set_Title (Value);
   end Set_Window_Title;

   function Key_Pressed (K : GLfw.Input.Keys.Key) return Boolean is
      use type Glfw.Input.Button_State;
   begin
      return Main_Window.Initialized and then
        Main_Window.Key_State (K) = Glfw.Input.Pressed;
   end Key_Pressed;

   function Window_Opened return Boolean is
   begin
      return Main_Window.Initialized and then Main_Window.Visible;
   end Window_Opened;

   procedure Close_Window is
   begin
      Main_Window.Destroy;
   end Close_Window;

   procedure Shutdown renames Glfw.Shutdown;

   procedure Configure_Minimum_OpenGL_Version (Major, Minor : Natural) is
   begin
      Glfw.Windows.Hints.Set_Minimum_OpenGL_Version (Major, Minor);
      -- needed for OSX
      if Major >= 3 then
         Glfw.Windows.Hints.Set_Forward_Compat (True);
         Glfw.Windows.Hints.Set_Profile (Glfw.Windows.Context.Core_Profile);
      end if;
   end Configure_Minimum_OpenGL_Version;

end GLFW_Utils;
