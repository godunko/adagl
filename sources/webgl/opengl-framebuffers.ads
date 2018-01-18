------------------------------------------------------------------------------
--                                                                          --
--                       Ada binding for OpenGL/WebGL                       --
--                                                                          --
--                        Runtime Library Component                         --
--                                                                          --
------------------------------------------------------------------------------
--                                                                          --
-- Copyright © 2016-2018, Vadim Godunko <vgodunko@gmail.com>                --
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
private with WebAPI.WebGL.Framebuffers;
private with WebAPI.WebGL.Rendering_Contexts;

with OpenGL.Renderbuffers;
with OpenGL.Textures;

package OpenGL.Framebuffers is

   pragma Preelaborate;

   type OpenGL_Framebuffer is tagged limited private;

   function Create (Self : in out OpenGL_Framebuffer'Class) return Boolean;
   procedure Create (Self : in out OpenGL_Framebuffer'Class);

   procedure Delete (Self : in out OpenGL_Framebuffer'Class);

   function Bind (Self : in out OpenGL_Framebuffer'Class) return Boolean;
   procedure Bind (Self : in out OpenGL_Framebuffer'Class);

   procedure Release (Self : in out OpenGL_Framebuffer'Class);

   procedure Read_Pixels
    (Self   : in out OpenGL_Framebuffer'Class;
     X      : OpenGL.GLint;
     Y      : OpenGL.GLint;
     Width  : OpenGL.GLsizei;
     Height : OpenGL.GLsizei;
     Data   : out OpenGL.GLubyte_Vector_4_Array);

   procedure Set_Renderbuffer
    (Self         : in out OpenGL_Framebuffer'Class;
     Renderbuffer : OpenGL.Renderbuffers.OpenGL_Renderbuffer'Class;
     Attachment   : OpenGL.GLenum);

   procedure Set_Texture
    (Self       : in out OpenGL_Framebuffer'Class;
     Texture    : OpenGL.Textures.OpenGL_Texture'Class;
     Attachment : OpenGL.GLenum);

private

   type OpenGL_Framebuffer is tagged limited record
      Framebuffer : WebAPI.WebGL.Framebuffers.WebGL_Framebuffer_Access;
      Context     :
        WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;
   end record;

end OpenGL.Framebuffers;
