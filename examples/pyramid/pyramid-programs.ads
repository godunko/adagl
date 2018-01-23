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
with OpenGL.Generic_Buffers;
with OpenGL.Programs;

package Pyramid.Programs is

   type Vertex_Data is record
      VP : OpenGL.Glfloat_Vector_3;
      TC : OpenGL.Glfloat_Vector_2;
   end record;

   type Vertex_Data_Array is array (Positive range <>) of Vertex_Data;
   pragma JavaScript_Typed_Array (Vertex_Data_Array);

   package Vertex_Data_Buffers is
     new OpenGL.Generic_Buffers (Vertex_Data, Positive, Vertex_Data_Array);

   type Pyramid_Program is new OpenGL.Programs.OpenGL_Program with private;

   procedure Initialize (Self : in out Pyramid_Program'Class);
   --  Initialize program object.

   procedure Set_Vertex_Data_Buffer
    (Self   : in out Pyramid_Program'Class;
     Buffer : Vertex_Data_Buffers.OpenGL_Buffer'Class);
   --  Sets buffer with data to draw.

   procedure Set_Texture_Unit
    (Self : in out Pyramid_Program'Class;
     Unit : OpenGL.Texture_Unit);
   --  Sets texture unit to be used to draw.

private

   type Pyramid_Program is new OpenGL.Programs.OpenGL_Program with record
      GS : OpenGL.Uniform_Location;
      VP : OpenGL.Attribute_Location;
      TC : OpenGL.Attribute_Location;
   end record;

   overriding function Link (Self : in out Pyramid_Program) return Boolean;
   --  Executed at link time. Link shaders into program and extracts locations
   --  of attributes.

end Pyramid.Programs;
