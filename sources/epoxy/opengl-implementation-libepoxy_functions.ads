--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Imlementation of the functions API on top of libepoxy.

with OpenGL.Functions;

package OpenGL.Implementation.libepoxy_Functions
  with Preelaborate
is

   type Epoxy_Functions is
     limited new OpenGL.Functions.OpenGL_Functions with null record;

   overriding procedure Blend_Func
     (Self               : Epoxy_Functions;
      Source_Factor      : OpenGL.GLenum;
      Destination_Factor : OpenGL.GLenum);

   overriding procedure Clear
     (Self : Epoxy_Functions;
      Mask : OpenGL.Clear_Buffer_Mask);

   overriding procedure Clear_Color
     (Self  : Epoxy_Functions;
      Red   : OpenGL.GLfloat;
      Green : OpenGL.GLfloat;
      Blue  : OpenGL.GLfloat;
      Alpha : OpenGL.GLfloat);

   procedure Clear_Depth
     (Self  : Epoxy_Functions;
      Depth : OpenGL.GLfloat);

   overriding procedure Depth_Func
     (Self : Epoxy_Functions;
      Func : OpenGL.GLenum);

   overriding procedure Disable
     (Self       : Epoxy_Functions;
      Capability : OpenGL.GLenum);

   overriding procedure Draw_Arrays
     (Self  : Epoxy_Functions;
      Mode  : OpenGL.GLenum;
      First : OpenGL.GLint;
      Count : OpenGL.GLsizei);

   procedure Draw_Elements
     (Self      : Epoxy_Functions;
      Mode      : OpenGL.GLenum;
      Count     : OpenGL.GLsizei;
      Data_Type : OpenGL.GLenum;
      Offset    : OpenGL.GLintptr);

   overriding procedure Enable
     (Self       : Epoxy_Functions;
      Capability : OpenGL.GLenum);

   overriding procedure Finish (Self : Epoxy_Functions);

   overriding procedure Flush (Self : Epoxy_Functions);

   overriding procedure Viewport
     (Self   : Epoxy_Functions;
      X      : OpenGL.GLint;
      Y      : OpenGL.GLint;
      Width  : OpenGL.GLsizei;
      Height : OpenGL.GLsizei);

end OpenGL.Implementation.libepoxy_Functions;
