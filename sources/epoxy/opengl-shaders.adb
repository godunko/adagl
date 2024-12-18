--
--  Copyright (C) 2018-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Interfaces.C.Strings;

with epoxy_gl_generated_h;
with OpenGL.Contexts.Internals;

package body OpenGL.Shaders is

   -------------------------
   -- Compile_Source_Code --
   -------------------------

   function Compile_Source_Code
    (Self   : in out OpenGL_Shader'Class;
     Source : League.Strings.Universal_String) return Boolean
   is
      use type Gdk.GLContext.Gdk_GLContext;
      use type epoxy.GLuint;

   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_Gdk_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Shader = 0 then
         Self.Shader :=
           epoxy_gl_generated_h.glCreateShader
            ((case Self.Shader_Type is
                when Vertex   => epoxy_gl_generated_h.GL_VERTEX_SHADER,
                when Fragment => epoxy_gl_generated_h.GL_FRAGMENT_SHADER));

         if Self.Shader = 0 then
            return False;
         end if;
      end if;

      declare
         Text : Interfaces.C.Strings.chars_ptr :=
           Interfaces.C.Strings.New_String (Source.To_UTF_8_String);
         Length : OpenGL.GLsizei :=
           OpenGL.GLsizei (Interfaces.C.Strings.Strlen (Text));
      begin
         epoxy_gl_generated_h.glShaderSource
           (Self.Shader, 1, (1 => Text), (1 => Length));
         Interfaces.C.Strings.Free (Text);
      end;

      epoxy_gl_generated_h.glCompileShader (Self.Shader);

      declare
         Status : aliased OpenGL.GLint;

      begin
         epoxy_gl_generated_h.glGetShaderiv
           (shader => Self.Shader,
            pname  => epoxy_gl_generated_h.GL_COMPILE_STATUS,
            params => Status);

         if Status = 0 then
            declare
               Buffer : Interfaces.C.char_array (1 .. 1024);
               Last   : aliased OpenGL.GLsizei;
            begin
               epoxy_gl_generated_h.glGetShaderInfoLog
                 (shader  => Self.Shader,
                  bufSize => Buffer'Length,
                  length  => Last,
                  infoLog => Buffer);

               epoxy_gl_generated_h.glDeleteShader (Self.Shader);
               Self.Shader  := 0;
               Self.Context := null;

               --  XXX Error handling must be implemented.

               raise Program_Error with "glCompileShader: " &
                 Interfaces.C.To_Ada
                   (Buffer (1 .. Interfaces.C.size_t (Last)), False);
            end;
         end if;

         return True;
      end;
   end Compile_Source_Code;

end OpenGL.Shaders;
