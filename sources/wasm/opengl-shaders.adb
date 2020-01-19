------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2020, Vadim Godunko <vgodunko@gmail.com>                --
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

package body OpenGL.Shaders is

   -------------------------
   -- Compile_Source_Code --
   -------------------------

   function Compile_Source_Code
    (Self   : in out OpenGL_Shader'Class;
     Source : Web.Strings.Web_String) return Boolean is
   begin
      if Self.Context.Is_Null then
         Self.Context := OpenGL.Contexts.Internals.Current_WebGL_Context;

         if Self.Context.Is_Null then
            return False;
         end if;
      end if;

      if Self.Shader.Is_Null then
         Self.Shader :=
           Self.Context.Create_Shader
            ((case Self.Shader_Type is
                when Vertex   => Web.GL.Rendering_Contexts.VERTEX_SHADER,
                when Fragment => Web.GL.Rendering_Contexts.FRAGMENT_SHADER));

         if Self.Shader.Is_Null then
            return False;
         end if;
      end if;

      Self.Context.Shader_Source (Self.Shader, Source);
      Self.Context.Compile_Shader (Self.Shader);

--  XXX Not implemented
--      if not Self.Context.Get_Shader_Parameter
--              (Self.Shader, WebAPI.WebGL.Rendering_Contexts.COMPILE_STATUS)
--      then
--         Self.Context.Delete_Shader (Self.Shader);
--         Self.Shader  := null;
--         Self.Context := null;
--
--         --  XXX Error handling must be implemented.
--
--         raise Program_Error;
--      end if;

      return True;
   end Compile_Source_Code;

end OpenGL.Shaders;
