------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2018, Vadim Godunko <vgodunko@gmail.com>                     --
-- All rights reserved.                                                     --
--                                                                          --
-- Redistribution and use in source and binary forms, with or without       --
-- modification, are permitted provided that the following conditions       --
-- are met:                                                                 --
--                                                                          --
--  * Redistributions of source code must retain the above copyright        --
--    notice, this list of conditions and the following disclaimer.         --
--                                                                          --
--  * Redistributions in binary form must reproduce the above copyright     --
--    notice, this list of conditions and the following disclaimer in the   --
--    documentation and/or other materials provided with the distribution.  --
--                                                                          --
--  * Neither the name of the Vadim Godunko, IE nor the names of its        --
--    contributors may be used to endorse or promote products derived from  --
--    this software without specific prior written permission.              --
--                                                                          --
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      --
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        --
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    --
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT     --
-- HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED --
-- TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR   --
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF   --
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING     --
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS       --
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.             --
--                                                                          --
------------------------------------------------------------------------------

with OpenGL.Contexts.Internals;

with Interfaces.C.Strings;

with GLEW; use GLEW;

package body OpenGL.Shaders is


   -------------------------
   -- Compile_Source_Code --
   -------------------------

   function Compile_Source_Code
    (Self   : in out OpenGL_Shader'Class;
     Source : League.Strings.Universal_String) return Boolean
   is

      use type GLEW.GLuint;
      use type GLFW.GLFWwindow_Access;

   begin
      if Self.Context = null then
         Self.Context := OpenGL.Contexts.Internals.Current_GLFW_Context;

         if Self.Context = null then
            return False;
         end if;
      end if;

      if Self.Shader = 0 then
         Self.Shader :=
           glCreateShader
            ((case Self.Shader_Type is
                when Vertex   => VERTEX_SHADER,
                when Fragment => FRAGMENT_SHADER));

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
         glShaderSource (Self.Shader, 1, (1 => Text), (1 => Length));
         Interfaces.C.Strings.Free (Text);
      end;

      glCompileShader (Self.Shader);

      declare
         Status : aliased OpenGL.GLint;
      begin
         glGetShaderiv (Self.Shader, COMPILE_STATUS, Status'Access);

         if Status = 0 then
            declare
               Buffer : Interfaces.C.char_array (1 .. 1024);
               Last   : aliased OpenGL.GLsizei;
            begin
               glGetShaderInfoLog
                 (Self.Shader, Buffer'Length, Last'Access, Buffer);

               glDeleteShader (Self.Shader);
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
