------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                            Examples Component                            --
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
with League.Characters.Latin;
with League.Strings;

package body Pyramid.Programs is

   use type League.Strings.Universal_String;

   Text_V : League.Strings.Universal_String :=
     League.Strings.To_Universal_String
       ("#version 130") & League.Characters.Latin.Line_Feed &
     "in vec3 vp;" &
     "in vec2 tc;" &
     "out vec2 TexCoord0;" &
     "void main() {" &
     "  gl_Position = vec4(vp, 1.0);" &
     "  TexCoord0 = tc;" &
     "}";
   Text_F : League.Strings.Universal_String :=
     League.Strings.To_Universal_String
       ("#version 130") & League.Characters.Latin.Line_Feed &
     "in vec2 TexCoord0;" &
     "out vec4 frag_colour;" &
     "uniform sampler2D gSampler;" &
     "void main() {" &
     "  frag_colour = texture2D(gSampler, TexCoord0.xy) +vec4(0.0, 0.0, 0.2, 0.2);" &
     "}";

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Pyramid_Program'Class) is
   begin
      Self.Add_Shader_From_Source_Code (OpenGL.Vertex, Text_V);
      Self.Add_Shader_From_Source_Code (OpenGL.Fragment, Text_F);
   end Initialize;

end Pyramid.Programs;
