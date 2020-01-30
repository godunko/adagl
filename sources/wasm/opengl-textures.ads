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

with Web.HTML.Images;
private with Web.GL.Rendering_Contexts;
private with Web.GL.Textures;

package OpenGL.Textures is

   pragma Preelaborate;

   type OpenGL_Texture (Texture_Type : OpenGL.Texture_Type) is
     tagged limited private;

   function Create (Self : in out OpenGL_Texture'Class) return Boolean;
   procedure Create (Self : in out OpenGL_Texture'Class);

   procedure Delete (Self : in out OpenGL_Texture'Class);

   function Is_Created (Self : OpenGL_Texture'Class) return Boolean;
   --  Returns True if the underlying OpenGL texture object has been created.

   procedure Bind (Self : in out OpenGL_Texture'Class);
   --  Binds this texture to the currently active texture unit ready for
   --  rendering.

--   function Bind
--    (Self : in out OpenGL_Texture'Class;
--     Unit : Texture_Unit) return Boolean;
--   procedure Bind
--    (Self : in out OpenGL_Texture'Class;
--     Unit : Texture_Unit);
--   --  Binds this texture to texture unit Unit ready for rendering.
--
--   procedure Release (Self : in out OpenGL_Texture'Class);

   procedure Set_Data
    (Self             : in out OpenGL_Texture'Class;
     Image            : Web.HTML.Images.HTML_Image_Element'Class;
     Generate_Mip_Map : Boolean := True);

--   procedure Set_Image_2D
--    (Self      : in out OpenGL_Texture'Class;
--     Level     : OpenGL.GLint;
--     Format    : OpenGL.GLenum;
--     Width     : OpenGL.GLsizei;
--     Height    : OpenGL.GLsizei;
--     Data_Type : OpenGL.GLenum);
--
--   procedure Set_Parameter
--    (Self      : in out OpenGL_Texture'Class;
--     Parameter : OpenGL.GLenum;
--     Value     : OpenGL.GLenum);

private

   type OpenGL_Texture (Texture_Type : OpenGL.Texture_Type) is
     tagged limited record
      Texture : Web.GL.Textures.WebGL_Texture;
      Context : Web.GL.Rendering_Contexts.WebGL_Rendering_Context;
   end record;

end OpenGL.Textures;
