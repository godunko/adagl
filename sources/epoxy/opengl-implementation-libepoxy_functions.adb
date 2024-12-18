--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with System.Storage_Elements;

with epoxy_gl_generated_h;

package body OpenGL.Implementation.libepoxy_Functions is

   ----------------
   -- Blend_Func --
   ----------------

   overriding procedure Blend_Func
     (Self               : Epoxy_Functions;
      Source_Factor      : OpenGL.GLenum;
      Destination_Factor : OpenGL.GLenum) is
   begin
      epoxy_gl_generated_h.glBlendFunc (Source_Factor, Destination_Factor);
   end Blend_Func;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self : Epoxy_Functions;
      Mask : OpenGL.Clear_Buffer_Mask) is
   begin
      epoxy_gl_generated_h.glClear (OpenGL.GLbitfield (Mask));
   end Clear;

   -----------------
   -- Clear_Color --
   -----------------

   overriding procedure Clear_Color
     (Self  : Epoxy_Functions;
      Red   : OpenGL.GLfloat;
      Green : OpenGL.GLfloat;
      Blue  : OpenGL.GLfloat;
      Alpha : OpenGL.GLfloat) is
   begin
      epoxy_gl_generated_h.glClearColor (Red, Green, Blue, Alpha);
   end Clear_Color;

   -----------------
   -- Clear_Depth --
   -----------------

   procedure Clear_Depth
     (Self  : Epoxy_Functions;
      Depth : OpenGL.GLfloat) is
   begin
      epoxy_gl_generated_h.glClearDepthf (Depth);
   end Clear_Depth;

   ----------------
   -- Depth_Func --
   ----------------

   overriding procedure Depth_Func
     (Self : Epoxy_Functions;
      Func : OpenGL.GLenum) is
   begin
      epoxy_gl_generated_h.glDepthFunc (Func);
   end Depth_Func;

   -------------
   -- Disable --
   -------------

   overriding procedure Disable
     (Self       : Epoxy_Functions;
      Capability : OpenGL.GLenum) is
   begin
      epoxy_gl_generated_h.glDisable (Capability);
   end Disable;

   -----------------
   -- Draw_Arrays --
   -----------------

   overriding procedure Draw_Arrays
     (Self  : Epoxy_Functions;
      Mode  : OpenGL.GLenum;
      First : OpenGL.GLint;
      Count : OpenGL.GLsizei) is
   begin
      epoxy_gl_generated_h.glDrawArrays (Mode, First, Count);
   end Draw_Arrays;

   -------------------
   -- Draw_Elements --
   -------------------

   procedure Draw_Elements
     (Self      : Epoxy_Functions;
      Mode      : OpenGL.GLenum;
      Count     : OpenGL.GLsizei;
      Data_Type : OpenGL.GLenum;
      Offset    : OpenGL.GLintptr) is
   begin
      epoxy_gl_generated_h.glDrawElements
        (Mode,
         Count,
         Data_Type,
         System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Offset)));
   end Draw_Elements;

   ------------
   -- Enable --
   ------------

   overriding procedure Enable
     (Self       : Epoxy_Functions;
      Capability : OpenGL.GLenum) is
   begin
      epoxy_gl_generated_h.glEnable (Capability);
   end Enable;

   ------------
   -- Finish --
   ------------

   overriding procedure Finish (Self : Epoxy_Functions) is
   begin
      epoxy_gl_generated_h.glFinish.all;
   end Finish;

   -----------
   -- Flush --
   -----------

   overriding procedure Flush (Self : Epoxy_Functions) is
   begin
      epoxy_gl_generated_h.glFlush.all;
   end Flush;

   --------------
   -- Viewport --
   --------------

   overriding procedure Viewport
     (Self   : Epoxy_Functions;
      X      : OpenGL.GLint;
      Y      : OpenGL.GLint;
      Width  : OpenGL.GLsizei;
      Height : OpenGL.GLsizei) is
   begin
      epoxy_gl_generated_h.glViewport (X, Y, Width, Height);
   end Viewport;

end OpenGL.Implementation.libepoxy_Functions;
