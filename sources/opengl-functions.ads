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

package OpenGL.Functions is

   pragma Preelaborate;

   type OpenGL_Functions is limited interface;

--   GL_APICALL void GL_APIENTRY glActiveTexture (GLenum texture);
--   GL_APICALL void GL_APIENTRY glAttachShader (GLuint program, GLuint shader);
--   GL_APICALL void GL_APIENTRY glBindAttribLocation (GLuint program, GLuint index, const GLchar *name);
--   GL_APICALL void GL_APIENTRY glBindRenderbuffer (GLenum target, GLuint renderbuffer);
--   GL_APICALL void GL_APIENTRY glBindTexture (GLenum target, GLuint texture);
--   GL_APICALL void GL_APIENTRY glBlendColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
--   GL_APICALL void GL_APIENTRY glBlendEquation (GLenum mode);
--   GL_APICALL void GL_APIENTRY glBlendEquationSeparate (GLenum modeRGB, GLenum modeAlpha);

   not overriding procedure Blend_Func
    (Self               : OpenGL_Functions;
     Source_Factor      : OpenGL.GLenum;
     Destination_Factor : OpenGL.GLenum) is abstract;

--   GL_APICALL void GL_APIENTRY glBlendFuncSeparate (GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha);
--   GL_APICALL void GL_APIENTRY glBufferSubData (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
--   GL_APICALL GLenum GL_APIENTRY glCheckFramebufferStatus (GLenum target);

   not overriding procedure Clear
    (Self : OpenGL_Functions;
     Mask : OpenGL.Clear_Buffer_Mask) is abstract;

   not overriding procedure Clear_Color
    (Self  : OpenGL_Functions;
     Red   : OpenGL.GLfloat;
     Green : OpenGL.GLfloat;
     Blue  : OpenGL.GLfloat;
     Alpha : OpenGL.GLfloat) is abstract;
--       with Pre'Class => Red in OpenGL.GLclampf'Range
--                           and Green in OpenGL.GLclampf'Range
--                           and Blue in OpenGL.GLclampf'Range
--                           and Alpha in OpenGL.GLclampf'Range;

--   GL_APICALL void GL_APIENTRY glClearDepthf (GLfloat d);
--   GL_APICALL void GL_APIENTRY glClearStencil (GLint s);
--   GL_APICALL void GL_APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
--   GL_APICALL void GL_APIENTRY glCompileShader (GLuint shader);
--   GL_APICALL void GL_APIENTRY glCompressedTexImage2D (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data);
--   GL_APICALL void GL_APIENTRY glCompressedTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data);
--   GL_APICALL void GL_APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
--   GL_APICALL void GL_APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
--   GL_APICALL GLuint GL_APIENTRY glCreateProgram (void);
--   GL_APICALL GLuint GL_APIENTRY glCreateShader (GLenum type);
--   GL_APICALL void GL_APIENTRY glCullFace (GLenum mode);
--   GL_APICALL void GL_APIENTRY glDeleteProgram (GLuint program);
--   GL_APICALL void GL_APIENTRY glDeleteRenderbuffers (GLsizei n, const GLuint *renderbuffers);
--   GL_APICALL void GL_APIENTRY glDeleteShader (GLuint shader);
--   GL_APICALL void GL_APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
--   GL_APICALL void GL_APIENTRY glDepthFunc (GLenum func);
--   GL_APICALL void GL_APIENTRY glDepthMask (GLboolean flag);
--   GL_APICALL void GL_APIENTRY glDepthRangef (GLfloat n, GLfloat f);
--   GL_APICALL void GL_APIENTRY glDetachShader (GLuint program, GLuint shader);

   not overriding procedure Disable
    (Self       : OpenGL_Functions;
     Capability : OpenGL.GLenum) is abstract;

--   GL_APICALL void GL_APIENTRY glDisableVertexAttribArray (GLuint index);

   not overriding procedure Draw_Arrays
    (Self  : OpenGL_Functions;
     Mode  : OpenGL.GLenum;
     First : OpenGL.GLint;
     Count : OpenGL.GLsizei) is abstract;

--   GL_APICALL void GL_APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const void *indices);

   not overriding procedure Enable
    (Self       : OpenGL_Functions;
     Capability : OpenGL.GLenum) is abstract;

--   GL_APICALL void GL_APIENTRY glEnableVertexAttribArray (GLuint index);

   not overriding procedure Finish (Self : OpenGL_Functions) is abstract;

   not overriding procedure Flush (Self : OpenGL_Functions) is abstract;

--   GL_APICALL void GL_APIENTRY glFrontFace (GLenum mode);
--   GL_APICALL void GL_APIENTRY glGenerateMipmap (GLenum target);
--   GL_APICALL void GL_APIENTRY glGenRenderbuffers (GLsizei n, GLuint *renderbuffers);
--   GL_APICALL void GL_APIENTRY glGenTextures (GLsizei n, GLuint *textures);
--   GL_APICALL void GL_APIENTRY glGetActiveAttrib (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
--   GL_APICALL void GL_APIENTRY glGetActiveUniform (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
--   GL_APICALL void GL_APIENTRY glGetAttachedShaders (GLuint program, GLsizei maxCount, GLsizei *count, GLuint *shaders);
--   GL_APICALL GLint GL_APIENTRY glGetAttribLocation (GLuint program, const GLchar *name);
--   GL_APICALL void GL_APIENTRY glGetBooleanv (GLenum pname, GLboolean *data);
--   GL_APICALL void GL_APIENTRY glGetBufferParameteriv (GLenum target, GLenum pname, GLint *params);
--   GL_APICALL GLenum GL_APIENTRY glGetError (void);
--   GL_APICALL void GL_APIENTRY glGetFloatv (GLenum pname, GLfloat *data);
--   GL_APICALL void GL_APIENTRY glGetFramebufferAttachmentParameteriv (GLenum target, GLenum attachment, GLenum pname, GLint *params);
--   GL_APICALL void GL_APIENTRY glGetIntegerv (GLenum pname, GLint *data);
--   GL_APICALL void GL_APIENTRY glGetProgramiv (GLuint program, GLenum pname, GLint *params);
--   GL_APICALL void GL_APIENTRY glGetProgramInfoLog (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
--   GL_APICALL void GL_APIENTRY glGetRenderbufferParameteriv (GLenum target, GLenum pname, GLint *params);
--   GL_APICALL void GL_APIENTRY glGetShaderiv (GLuint shader, GLenum pname, GLint *params);
--   GL_APICALL void GL_APIENTRY glGetShaderInfoLog (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
--   GL_APICALL void GL_APIENTRY glGetShaderPrecisionFormat (GLenum shadertype, GLenum precisiontype, GLint *range, GLint *precision);
--   GL_APICALL void GL_APIENTRY glGetShaderSource (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source);
--   GL_APICALL const GLubyte *GL_APIENTRY glGetString (GLenum name);
--   GL_APICALL void GL_APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
--   GL_APICALL void GL_APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
--   GL_APICALL void GL_APIENTRY glGetUniformfv (GLuint program, GLint location, GLfloat *params);
--   GL_APICALL void GL_APIENTRY glGetUniformiv (GLuint program, GLint location, GLint *params);
--   GL_APICALL GLint GL_APIENTRY glGetUniformLocation (GLuint program, const GLchar *name);
--   GL_APICALL void GL_APIENTRY glGetVertexAttribfv (GLuint index, GLenum pname, GLfloat *params);
--   GL_APICALL void GL_APIENTRY glGetVertexAttribiv (GLuint index, GLenum pname, GLint *params);
--   GL_APICALL void GL_APIENTRY glGetVertexAttribPointerv (GLuint index, GLenum pname, void **pointer);
--   GL_APICALL void GL_APIENTRY glHint (GLenum target, GLenum mode);
--   GL_APICALL GLboolean GL_APIENTRY glIsBuffer (GLuint buffer);
--   GL_APICALL GLboolean GL_APIENTRY glIsEnabled (GLenum cap);
--   GL_APICALL GLboolean GL_APIENTRY glIsFramebuffer (GLuint framebuffer);
--   GL_APICALL GLboolean GL_APIENTRY glIsProgram (GLuint program);
--   GL_APICALL GLboolean GL_APIENTRY glIsRenderbuffer (GLuint renderbuffer);
--   GL_APICALL GLboolean GL_APIENTRY glIsShader (GLuint shader);
--   GL_APICALL GLboolean GL_APIENTRY glIsTexture (GLuint texture);
--   GL_APICALL void GL_APIENTRY glLineWidth (GLfloat width);
--   GL_APICALL void GL_APIENTRY glLinkProgram (GLuint program);
--   GL_APICALL void GL_APIENTRY glPixelStorei (GLenum pname, GLint param);
--   GL_APICALL void GL_APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
--   GL_APICALL void GL_APIENTRY glReleaseShaderCompiler (void);
--   GL_APICALL void GL_APIENTRY glRenderbufferStorage (GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
--   GL_APICALL void GL_APIENTRY glSampleCoverage (GLfloat value, GLboolean invert);
--   GL_APICALL void GL_APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
--   GL_APICALL void GL_APIENTRY glShaderBinary (GLsizei count, const GLuint *shaders, GLenum binaryformat, const void *binary, GLsizei length);
--   GL_APICALL void GL_APIENTRY glShaderSource (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
--   GL_APICALL void GL_APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
--   GL_APICALL void GL_APIENTRY glStencilFuncSeparate (GLenum face, GLenum func, GLint ref, GLuint mask);
--   GL_APICALL void GL_APIENTRY glStencilMask (GLuint mask);
--   GL_APICALL void GL_APIENTRY glStencilMaskSeparate (GLenum face, GLuint mask);
--   GL_APICALL void GL_APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
--   GL_APICALL void GL_APIENTRY glStencilOpSeparate (GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass);
--   GL_APICALL void GL_APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
--   GL_APICALL void GL_APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
--   GL_APICALL void GL_APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
--   GL_APICALL void GL_APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
--   GL_APICALL void GL_APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
--   GL_APICALL void GL_APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
--   GL_APICALL void GL_APIENTRY glUniform1f (GLint location, GLfloat v0);
--   GL_APICALL void GL_APIENTRY glUniform1fv (GLint location, GLsizei count, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUniform1i (GLint location, GLint v0);
--   GL_APICALL void GL_APIENTRY glUniform1iv (GLint location, GLsizei count, const GLint *value);
--   GL_APICALL void GL_APIENTRY glUniform2f (GLint location, GLfloat v0, GLfloat v1);
--   GL_APICALL void GL_APIENTRY glUniform2fv (GLint location, GLsizei count, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUniform2i (GLint location, GLint v0, GLint v1);
--   GL_APICALL void GL_APIENTRY glUniform2iv (GLint location, GLsizei count, const GLint *value);
--   GL_APICALL void GL_APIENTRY glUniform3f (GLint location, GLfloat v0, GLfloat v1, GLfloat v2);
--   GL_APICALL void GL_APIENTRY glUniform3fv (GLint location, GLsizei count, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUniform3i (GLint location, GLint v0, GLint v1, GLint v2);
--   GL_APICALL void GL_APIENTRY glUniform3iv (GLint location, GLsizei count, const GLint *value);
--   GL_APICALL void GL_APIENTRY glUniform4f (GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
--   GL_APICALL void GL_APIENTRY glUniform4fv (GLint location, GLsizei count, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUniform4i (GLint location, GLint v0, GLint v1, GLint v2, GLint v3);
--   GL_APICALL void GL_APIENTRY glUniform4iv (GLint location, GLsizei count, const GLint *value);
--   GL_APICALL void GL_APIENTRY glUniformMatrix2fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUniformMatrix3fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUniformMatrix4fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
--   GL_APICALL void GL_APIENTRY glUseProgram (GLuint program);
--   GL_APICALL void GL_APIENTRY glValidateProgram (GLuint program);
--   GL_APICALL void GL_APIENTRY glVertexAttrib1f (GLuint index, GLfloat x);
--   GL_APICALL void GL_APIENTRY glVertexAttrib1fv (GLuint index, const GLfloat *v);
--   GL_APICALL void GL_APIENTRY glVertexAttrib2f (GLuint index, GLfloat x, GLfloat y);
--   GL_APICALL void GL_APIENTRY glVertexAttrib2fv (GLuint index, const GLfloat *v);
--   GL_APICALL void GL_APIENTRY glVertexAttrib3f (GLuint index, GLfloat x, GLfloat y, GLfloat z);
--   GL_APICALL void GL_APIENTRY glVertexAttrib3fv (GLuint index, const GLfloat *v);
--   GL_APICALL void GL_APIENTRY glVertexAttrib4f (GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
--   GL_APICALL void GL_APIENTRY glVertexAttrib4fv (GLuint index, const GLfloat *v);
--   GL_APICALL void GL_APIENTRY glVertexAttribPointer (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);

   not overriding procedure Viewport
    (Self   : OpenGL_Functions;
     X      : OpenGL.GLint;
     Y      : OpenGL.GLint;
     Width  : OpenGL.GLsizei;
     Height : OpenGL.GLsizei) is abstract;

--            <command name="glActiveTexture"/>
--            <command name="glAttachShader"/>
--            <command name="glBindAttribLocation"/>
--            <command name="glBindRenderbuffer"/>
--            <command name="glBindTexture"/>
--            <command name="glBlendColor"/>
--            <command name="glBlendEquation"/>
--            <command name="glBlendEquationSeparate"/>
--            <command name="glBlendFuncSeparate"/>
--            <command name="glBufferSubData"/>
--            <command name="glCheckFramebufferStatus"/>
--            <command name="glClearDepthf"/>
--            <command name="glClearStencil"/>
--            <command name="glColorMask"/>
--            <command name="glCompileShader"/>
--            <command name="glCompressedTexImage2D"/>
--            <command name="glCompressedTexSubImage2D"/>
--            <command name="glCopyTexImage2D"/>
--            <command name="glCopyTexSubImage2D"/>
--            <command name="glCreateProgram"/>
--            <command name="glCreateShader"/>
--            <command name="glCullFace"/>
--            <command name="glDeleteProgram"/>
--            <command name="glDeleteRenderbuffers"/>
--            <command name="glDeleteShader"/>
--            <command name="glDeleteTextures"/>
--            <command name="glDepthFunc"/>
--            <command name="glDepthMask"/>
--            <command name="glDepthRangef"/>
--            <command name="glDetachShader"/>
--            <command name="glDisableVertexAttribArray"/>
--            <command name="glDrawArrays"/>
--            <command name="glDrawElements"/>
--            <command name="glEnableVertexAttribArray"/>
--            <command name="glFinish"/>
--            <command name="glFlush"/>
--            <command name="glFrontFace"/>
--            <command name="glGenerateMipmap"/>
--            <command name="glGenRenderbuffers"/>
--            <command name="glGenTextures"/>
--            <command name="glGetActiveAttrib"/>
--            <command name="glGetActiveUniform"/>
--            <command name="glGetAttachedShaders"/>
--            <command name="glGetAttribLocation"/>
--            <command name="glGetBooleanv"/>
--            <command name="glGetBufferParameteriv"/>
--            <command name="glGetError"/>
--            <command name="glGetFloatv"/>
--            <command name="glGetFramebufferAttachmentParameteriv"/>
--            <command name="glGetIntegerv"/>
--            <command name="glGetProgramiv"/>
--            <command name="glGetProgramInfoLog"/>
--            <command name="glGetRenderbufferParameteriv"/>
--            <command name="glGetShaderiv"/>
--            <command name="glGetShaderInfoLog"/>
--            <command name="glGetShaderPrecisionFormat"/>
--            <command name="glGetShaderSource"/>
--            <command name="glGetString"/>
--            <command name="glGetTexParameterfv"/>
--            <command name="glGetTexParameteriv"/>
--            <command name="glGetUniformfv"/>
--            <command name="glGetUniformiv"/>
--            <command name="glGetUniformLocation"/>
--            <command name="glGetVertexAttribfv"/>
--            <command name="glGetVertexAttribiv"/>
--            <command name="glGetVertexAttribPointerv"/>
--            <command name="glHint"/>
--            <command name="glIsBuffer"/>
--            <command name="glIsEnabled"/>
--            <command name="glIsFramebuffer"/>
--            <command name="glIsProgram"/>
--            <command name="glIsRenderbuffer"/>
--            <command name="glIsShader"/>
--            <command name="glIsTexture"/>
--            <command name="glLineWidth"/>
--            <command name="glLinkProgram"/>
--            <command name="glPixelStorei"/>
--            <command name="glPolygonOffset"/>
--            <command name="glReleaseShaderCompiler"/>
--            <command name="glRenderbufferStorage"/>
--            <command name="glSampleCoverage"/>
--            <command name="glScissor"/>
--            <command name="glShaderBinary"/>
--            <command name="glShaderSource"/>
--            <command name="glStencilFunc"/>
--            <command name="glStencilFuncSeparate"/>
--            <command name="glStencilMask"/>
--            <command name="glStencilMaskSeparate"/>
--            <command name="glStencilOp"/>
--            <command name="glStencilOpSeparate"/>
--            <command name="glTexImage2D"/>
--            <command name="glTexParameterf"/>
--            <command name="glTexParameterfv"/>
--            <command name="glTexParameteri"/>
--            <command name="glTexParameteriv"/>
--            <command name="glTexSubImage2D"/>
--            <command name="glUniform1f"/>
--            <command name="glUniform1fv"/>
--            <command name="glUniform1i"/>
--            <command name="glUniform1iv"/>
--            <command name="glUniform2f"/>
--            <command name="glUniform2fv"/>
--            <command name="glUniform2i"/>
--            <command name="glUniform2iv"/>
--            <command name="glUniform3f"/>
--            <command name="glUniform3fv"/>
--            <command name="glUniform3i"/>
--            <command name="glUniform3iv"/>
--            <command name="glUniform4f"/>
--            <command name="glUniform4fv"/>
--            <command name="glUniform4i"/>
--            <command name="glUniform4iv"/>
--            <command name="glUniformMatrix2fv"/>
--            <command name="glUniformMatrix3fv"/>
--            <command name="glUniformMatrix4fv"/>
--            <command name="glUseProgram"/>
--            <command name="glValidateProgram"/>
--            <command name="glVertexAttrib1f"/>
--            <command name="glVertexAttrib1fv"/>
--            <command name="glVertexAttrib2f"/>
--            <command name="glVertexAttrib2fv"/>
--            <command name="glVertexAttrib3f"/>
--            <command name="glVertexAttrib3fv"/>
--            <command name="glVertexAttrib4f"/>
--            <command name="glVertexAttrib4fv"/>
--            <command name="glVertexAttribPointer"/>

   --  Some functions are available as subprograms for objects of tagged
   --  types from other packages:
   --
   --  OpenGL.Framebuffers:
   --   - glBindFramebuffer
   --   - glDeleteFramebuffers
   --   - glFramebufferRenderbuffer
   --   - glFramebufferTexture2D
   --   - glGenFramebuffers
   --   - glReadPixels
   --
   --  OpenGL.Generic_Buffers:
   --   - glBindBuffer
   --   - glBufferData
   --   - glDeleteBuffers
   --   - glGenBuffers

end OpenGL.Functions;
