--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with League.Strings;

private with Gdk.GLContext;

private with epoxy;

package OpenGL.Shaders is

   type OpenGL_Shader (Shader_Type : OpenGL.Shader_Type) is
     tagged limited private;

   type OpenGL_Shader_Access is access all OpenGL_Shader'Class;

   function Compile_Source_Code
    (Self   : in out OpenGL_Shader'Class;
     Source : League.Strings.Universal_String) return Boolean;
   --  Sets the source code for this shader and compiles it. Returns True if
   --  the source was successfully compiled, False otherwise.

private

   type OpenGL_Shader (Shader_Type : OpenGL.Shader_Type) is
     tagged limited record
      Context : Gdk.GLContext.Gdk_GLContext;
      Shader  : aliased epoxy.GLuint := 0;
   end record;

end OpenGL.Shaders;
