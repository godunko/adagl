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
with Interfaces;

private with WebAPI.WebGL.Uniform_Locations;

package OpenGL is

   pragma Preelaborate;

   use type Interfaces.Integer_32;

   ------------------------------------
   -- Integer, fixed and float types --
   ------------------------------------

   type GLbyte is new Interfaces.Integer_8;
   type GLubyte is new Interfaces.Unsigned_8;
   type GLshort is new Interfaces.Integer_16;
   type GLushort is new Interfaces.Unsigned_16;
   type GLfixed is private;

   type GLint is new Interfaces.Integer_32;
   subtype GLsizei is GLint range 0 .. GLint'Last;

   type GLfloat is new Interfaces.IEEE_Float_32;
   subtype GLclampf is GLfloat range 0.0 .. 1.0;

--        <type api="gles2" requires="khrplatform">typedef khronos_int64_t <name>GLint64</name>;</type>
--        <type api="gles2" requires="khrplatform">typedef khronos_uint64_t <name>GLuint64</name>;</type>
--        <type api="gles2" requires="khrplatform">typedef khronos_int64_t <name>GLint64EXT</name>;</type>
--        <type api="gles2" requires="khrplatform">typedef khronos_uint64_t <name>GLuint64EXT</name>;</type>
--        <type api="gles2" requires="khrplatform">typedef khronos_intptr_t <name>GLintptr</name>;</type>
--        <type api="gles2" requires="khrplatform">typedef khronos_ssize_t <name>GLsizeiptr</name>;</type>

   --------------------------
   -- Vectors and matrices --
   --------------------------

   --  Components of all arrays should be aliased. It doesn't work for A2JS,
   --  so declarations for native and WebGL platforms are little bit different.

   type GLbyte_Vector_2 is array (Positive range 1 .. 2) of GLbyte;
--   pragma JavaScript_Typed_Array (GLbyte_Vector_2);
   type GLbyte_Vector_3 is array (Positive range 1 .. 3) of GLbyte;
--   pragma JavaScript_Typed_Array (GLbyte_Vector_3);
   type GLbyte_Vector_4 is array (Positive range 1 .. 4) of GLbyte;
--   pragma JavaScript_Typed_Array (GLbyte_Vector_4);

   type GLubyte_Vector_2 is array (Positive range 1 .. 2) of GLubyte;
--   pragma JavaScript_Typed_Array (GLubyte_Vector_2);
   type GLubyte_Vector_3 is array (Positive range 1 .. 3) of GLubyte;
--   pragma JavaScript_Typed_Array (GLubyte_Vector_3);
   type GLubyte_Vector_4 is array (Positive range 1 .. 4) of GLubyte;
--   pragma JavaScript_Typed_Array (GLubyte_Vector_4);

   type GLshort_Vector_2 is array (Positive range 1 .. 2) of GLshort;
--   pragma JavaScript_Typed_Array (GLshort_Vector_2);
   type GLshort_Vector_3 is array (Positive range 1 .. 3) of GLshort;
--   pragma JavaScript_Typed_Array (GLshort_Vector_3);
   type GLshort_Vector_4 is array (Positive range 1 .. 4) of GLshort;
--   pragma JavaScript_Typed_Array (GLshort_Vector_4);

   type GLushort_Vector_2 is array (Positive range 1 .. 2) of GLushort;
--   pragma JavaScript_Typed_Array (GLushort_Vector_2);
   type GLushort_Vector_3 is array (Positive range 1 .. 3) of GLushort;
--   pragma JavaScript_Typed_Array (GLushort_Vector_3);
   type GLushort_Vector_4 is array (Positive range 1 .. 4) of GLushort;
--   pragma JavaScript_Typed_Array (GLushort_Vector_4);

   type GLfixed_Vector_2 is array (Positive range 1 .. 2) of GLfixed;
--   pragma JavaScript_Typed_Array (GLfixed_Vector_2);
   type GLfixed_Vector_3 is array (Positive range 1 .. 3) of GLfixed;
--   pragma JavaScript_Typed_Array (GLfixed_Vector_3);
   type GLfixed_Vector_4 is array (Positive range 1 .. 4) of GLfixed;
--   pragma JavaScript_Typed_Array (GLfixed_Vector_4);

   type GLfloat_Vector_2 is array (Positive range 1 .. 2) of GLfloat;
--   pragma JavaScript_Typed_Array (GLfloat_Vector_2);
   type GLfloat_Vector_3 is array (Positive range 1 .. 3) of GLfloat;
--   pragma JavaScript_Typed_Array (GLfloat_Vector_3);
   type GLfloat_Vector_4 is array (Positive range 1 .. 4) of GLfloat;
--   pragma JavaScript_Typed_Array (GLfloat_Vector_4);

   type GLbyte_Matrix_2x2 is
     array (Positive range 1 .. 2, Positive range 1 .. 2) of GLbyte
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLbyte_Matrix_2x2);
   type GLbyte_Matrix_3x3 is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of GLbyte
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLbyte_Matrix_3x3);
   type GLbyte_Matrix_4x4 is
     array (Positive range 1 .. 4, Positive range 1 .. 4) of GLbyte
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLbyte_Matrix_4x4);

   type GLubyte_Matrix_2x2 is
     array (Positive range 1 .. 2, Positive range 1 .. 2) of GLubyte
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLubyte_Matrix_2x2);
   type GLubyte_Matrix_3x3 is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of GLubyte
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLubyte_Matrix_3x3);
   type GLubyte_Matrix_4x4 is
     array (Positive range 1 .. 4, Positive range 1 .. 4) of GLubyte
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLubyte_Matrix_4x4);

   type GLshort_Matrix_2x2 is
     array (Positive range 1 .. 2, Positive range 1 .. 2) of GLshort
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLshort_Matrix_2x2);
   type GLshort_Matrix_3x3 is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of GLshort
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLshort_Matrix_3x3);
   type GLshort_Matrix_4x4 is
     array (Positive range 1 .. 4, Positive range 1 .. 4) of GLshort
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLshort_Matrix_4x4);

   type GLushort_Matrix_2x2 is
     array (Positive range 1 .. 2, Positive range 1 .. 2) of GLushort
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLushort_Matrix_2x2);
   type GLushort_Matrix_3x3 is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of GLushort
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLushort_Matrix_3x3);
   type GLushort_Matrix_4x4 is
     array (Positive range 1 .. 4, Positive range 1 .. 4) of GLushort
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLushort_Matrix_4x4);

   type GLfixed_Matrix_2x2 is
     array (Positive range 1 .. 2, Positive range 1 .. 2) of GLfixed
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLfixed_Matrix_2x2);
   type GLfixed_Matrix_3x3 is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of GLfixed
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLfixed_Matrix_3x3);
   type GLfixed_Matrix_4x4 is
     array (Positive range 1 .. 4, Positive range 1 .. 4) of GLfixed
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLfixed_Matrix_4x4);

   type GLfloat_Matrix_2x2 is
     array (Positive range 1 .. 2, Positive range 1 .. 2) of GLfloat
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLfloat_Matrix_2x2);
   type GLfloat_Matrix_3x3 is
     array (Positive range 1 .. 3, Positive range 1 .. 3) of GLfloat
       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLfloat_Matrix_3x3);
   type GLfloat_Matrix_4x4 is
     array (Positive range 1 .. 4) of GLfloat_Vector_4
       with Convention => Fortran;
--     array (Positive range 1 .. 4, Positive range 1 .. 4) of GLfloat
--       with Convention => Fortran;
--   pragma JavaScript_Typed_Array (GLfloat_Matrix_4x4);

   type GLubyte_Vector_4_Array is
     array (Positive range <>) of GLubyte_Vector_4;

   --------------
   -- Bitfield --
   --------------

   type GLbitfield is private;

   GL_DEPTH_BUFFER_BIT   : constant GLbitfield;
   GL_STENCIL_BUFFER_BIT : constant GLbitfield;
   GL_COLOR_BUFFER_BIT   : constant GLbitfield;

   -----------------
   -- Enumeration --
   -----------------

   type GLenum is private;

   GL_BYTE           : constant GLenum;
   GL_UNSIGNED_BYTE  : constant GLenum;
   GL_SHORT          : constant GLenum;
   GL_UNSIGNED_SHORT : constant GLenum;
   GL_FIXED          : constant GLenum;
   GL_FLOAT          : constant GLenum;

   GL_POINTS         : constant GLenum;
   GL_LINES          : constant GLenum;
   GL_LINE_LOOP      : constant GLenum;
   GL_LINE_STRIP     : constant GLenum;
   GL_TRIANGLES      : constant GLenum;
   GL_TRIANGLE_STRIP : constant GLenum;
   GL_TRIANGLE_FAN   : constant GLenum;

   GL_COLOR_ATTACHMENT0  : constant GLenum;
   GL_DEPTH_ATTACHMENT   : constant GLenum;
   GL_STENCIL_ATTACHMENT : constant GLenum;

   GL_TEXTURE_2D                  : constant GLenum;
   GL_TEXTURE_CUBE_MAP_POSITIVE_X : constant GLenum;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X : constant GLenum;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y : constant GLenum;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y : constant GLenum;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z : constant GLenum;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z : constant GLenum;

   GL_RGBA4             : constant GLenum;
   GL_RGB5_A1           : constant GLenum;
   GL_RGB565            : constant GLenum;
   GL_DEPTH_COMPONENT16 : constant GLenum;
   GL_STENCIL_INDEX8    : constant GLenum;

   GL_ALPHA           : constant GLenum;
   GL_LUMINANCE       : constant GLenum;
   GL_LUMINANCE_ALPHA : constant GLenum;
   GL_RGB             : constant GLenum;
   GL_RGBA            : constant GLenum;

   GL_UNSIGNED_SHORT_4_4_4_4 : constant GLenum;
   GL_UNSIGNED_SHORT_5_5_5_1 : constant GLenum;
   GL_UNSIGNED_SHORT_5_6_5   : constant GLenum;

   GL_TEXTURE_MAG_FILTER : constant GLenum;
   GL_TEXTURE_MIN_FILTER : constant GLenum;
   GL_TEXTURE_WRAP_S     : constant GLenum;
   GL_TEXTURE_WRAP_T     : constant GLenum;

   GL_NEAREST                : constant GLenum;
   GL_LINEAR                 : constant GLenum;
   GL_NEAREST_MIPMAP_NEAREST : constant GLenum;
   GL_LINEAR_MIPMAP_NEAREST  : constant GLenum;
   GL_NEAREST_MIPMAP_LINEAR  : constant GLenum;
   GL_LINEAR_MIPMAP_LINEAR   : constant GLenum;

   GL_REPEAT          : constant GLenum;
   GL_CLAMP_TO_EDGE   : constant GLenum;
   GL_MIRRORED_REPEAT : constant GLenum;

   GL_CULL_FACE                : constant GLenum;
   GL_BLEND                    : constant GLenum;
   GL_DITHER                   : constant GLenum;
   GL_STENCIL_TEST             : constant GLenum;
   GL_DEPTH_TEST               : constant GLenum;
   GL_SCISSOR_TEST             : constant GLenum;
   GL_POLYGON_OFFSET_FILL      : constant GLenum;
   GL_SAMPLE_ALPHA_TO_COVERAGE : constant GLenum;
   GL_SAMPLE_COVERAGE          : constant GLenum;

   GL_ZERO                     : constant GLenum;
   GL_ONE                      : constant GLenum;
   GL_SRC_COLOR                : constant GLenum;
   GL_ONE_MINUS_SRC_COLOR      : constant GLenum;
   GL_DST_COLOR                : constant GLenum;
   GL_ONE_MINUS_DST_COLOR      : constant GLenum;
   GL_SRC_ALPHA                : constant GLenum;
   GL_ONE_MINUS_SRC_ALPHA      : constant GLenum;
   GL_DST_ALPHA                : constant GLenum;
   GL_ONE_MINUS_DST_ALPHA      : constant GLenum;
   GL_CONSTANT_COLOR           : constant GLenum;
   GL_ONE_MINUS_CONSTANT_COLOR : constant GLenum;
   GL_CONSTANT_ALPHA           : constant GLenum;
   GL_ONE_MINUS_CONSTANT_ALPHA : constant GLenum;
   GL_SRC_ALPHA_SATURATE       : constant GLenum;

--            <enum name="GL_FALSE"/>
--            <enum name="GL_TRUE"/>
--            <enum name="GL_FUNC_ADD"/>
--            <enum name="GL_BLEND_EQUATION"/>
--            <enum name="GL_BLEND_EQUATION_RGB"/>
--            <enum name="GL_BLEND_EQUATION_ALPHA"/>
--            <enum name="GL_FUNC_SUBTRACT"/>
--            <enum name="GL_FUNC_REVERSE_SUBTRACT"/>
--            <enum name="GL_BLEND_DST_RGB"/>
--            <enum name="GL_BLEND_SRC_RGB"/>
--            <enum name="GL_BLEND_DST_ALPHA"/>
--            <enum name="GL_BLEND_SRC_ALPHA"/>
--            <enum name="GL_BLEND_COLOR"/>
--            <enum name="GL_ARRAY_BUFFER"/>
--            <enum name="GL_ELEMENT_ARRAY_BUFFER"/>
--            <enum name="GL_ARRAY_BUFFER_BINDING"/>
--            <enum name="GL_ELEMENT_ARRAY_BUFFER_BINDING"/>
--            <enum name="GL_STREAM_DRAW"/>
--            <enum name="GL_STATIC_DRAW"/>
--            <enum name="GL_DYNAMIC_DRAW"/>
--            <enum name="GL_BUFFER_SIZE"/>
--            <enum name="GL_BUFFER_USAGE"/>
--            <enum name="GL_CURRENT_VERTEX_ATTRIB"/>
--            <enum name="GL_FRONT"/>
--            <enum name="GL_BACK"/>
--            <enum name="GL_FRONT_AND_BACK"/>
--            <enum name="GL_NO_ERROR"/>
--            <enum name="GL_INVALID_ENUM"/>
--            <enum name="GL_INVALID_VALUE"/>
--            <enum name="GL_INVALID_OPERATION"/>
--            <enum name="GL_OUT_OF_MEMORY"/>
--            <enum name="GL_CW"/>
--            <enum name="GL_CCW"/>
--            <enum name="GL_LINE_WIDTH"/>
--            <enum name="GL_ALIASED_POINT_SIZE_RANGE"/>
--            <enum name="GL_ALIASED_LINE_WIDTH_RANGE"/>
--            <enum name="GL_CULL_FACE_MODE"/>
--            <enum name="GL_FRONT_FACE"/>
--            <enum name="GL_DEPTH_RANGE"/>
--            <enum name="GL_DEPTH_WRITEMASK"/>
--            <enum name="GL_DEPTH_CLEAR_VALUE"/>
--            <enum name="GL_DEPTH_FUNC"/>
--            <enum name="GL_STENCIL_CLEAR_VALUE"/>
--            <enum name="GL_STENCIL_FUNC"/>
--            <enum name="GL_STENCIL_FAIL"/>
--            <enum name="GL_STENCIL_PASS_DEPTH_FAIL"/>
--            <enum name="GL_STENCIL_PASS_DEPTH_PASS"/>
--            <enum name="GL_STENCIL_REF"/>
--            <enum name="GL_STENCIL_VALUE_MASK"/>
--            <enum name="GL_STENCIL_WRITEMASK"/>
--            <enum name="GL_STENCIL_BACK_FUNC"/>
--            <enum name="GL_STENCIL_BACK_FAIL"/>
--            <enum name="GL_STENCIL_BACK_PASS_DEPTH_FAIL"/>
--            <enum name="GL_STENCIL_BACK_PASS_DEPTH_PASS"/>
--            <enum name="GL_STENCIL_BACK_REF"/>
--            <enum name="GL_STENCIL_BACK_VALUE_MASK"/>
--            <enum name="GL_STENCIL_BACK_WRITEMASK"/>
--            <enum name="GL_VIEWPORT"/>
--            <enum name="GL_SCISSOR_BOX"/>
--            <enum name="GL_COLOR_CLEAR_VALUE"/>
--            <enum name="GL_COLOR_WRITEMASK"/>
--            <enum name="GL_UNPACK_ALIGNMENT"/>
--            <enum name="GL_PACK_ALIGNMENT"/>
--            <enum name="GL_MAX_TEXTURE_SIZE"/>
--            <enum name="GL_MAX_VIEWPORT_DIMS"/>
--            <enum name="GL_SUBPIXEL_BITS"/>
--            <enum name="GL_RED_BITS"/>
--            <enum name="GL_GREEN_BITS"/>
--            <enum name="GL_BLUE_BITS"/>
--            <enum name="GL_ALPHA_BITS"/>
--            <enum name="GL_DEPTH_BITS"/>
--            <enum name="GL_STENCIL_BITS"/>
--            <enum name="GL_POLYGON_OFFSET_UNITS"/>
--            <enum name="GL_POLYGON_OFFSET_FACTOR"/>
--            <enum name="GL_TEXTURE_BINDING_2D"/>
--            <enum name="GL_SAMPLE_BUFFERS"/>
--            <enum name="GL_SAMPLES"/>
--            <enum name="GL_SAMPLE_COVERAGE_VALUE"/>
--            <enum name="GL_SAMPLE_COVERAGE_INVERT"/>
--            <enum name="GL_NUM_COMPRESSED_TEXTURE_FORMATS"/>
--            <enum name="GL_COMPRESSED_TEXTURE_FORMATS"/>
--            <enum name="GL_DONT_CARE"/>
--            <enum name="GL_FASTEST"/>
--            <enum name="GL_NICEST"/>
--            <enum name="GL_GENERATE_MIPMAP_HINT"/>
--            <enum name="GL_INT"/>
--            <enum name="GL_UNSIGNED_INT"/>
--            <enum name="GL_DEPTH_COMPONENT"/>
--            <enum name="GL_FRAGMENT_SHADER"/>
--            <enum name="GL_VERTEX_SHADER"/>
--            <enum name="GL_MAX_VERTEX_ATTRIBS"/>
--            <enum name="GL_MAX_VERTEX_UNIFORM_VECTORS"/>
--            <enum name="GL_MAX_VARYING_VECTORS"/>
--            <enum name="GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS"/>
--            <enum name="GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS"/>
--            <enum name="GL_MAX_TEXTURE_IMAGE_UNITS"/>
--            <enum name="GL_MAX_FRAGMENT_UNIFORM_VECTORS"/>
--            <enum name="GL_SHADER_TYPE"/>
--            <enum name="GL_DELETE_STATUS"/>
--            <enum name="GL_LINK_STATUS"/>
--            <enum name="GL_VALIDATE_STATUS"/>
--            <enum name="GL_ATTACHED_SHADERS"/>
--            <enum name="GL_ACTIVE_UNIFORMS"/>
--            <enum name="GL_ACTIVE_UNIFORM_MAX_LENGTH"/>
--            <enum name="GL_ACTIVE_ATTRIBUTES"/>
--            <enum name="GL_ACTIVE_ATTRIBUTE_MAX_LENGTH"/>
--            <enum name="GL_SHADING_LANGUAGE_VERSION"/>
--            <enum name="GL_CURRENT_PROGRAM"/>
--            <enum name="GL_NEVER"/>
--            <enum name="GL_LESS"/>
--            <enum name="GL_EQUAL"/>
--            <enum name="GL_LEQUAL"/>
--            <enum name="GL_GREATER"/>
--            <enum name="GL_NOTEQUAL"/>
--            <enum name="GL_GEQUAL"/>
--            <enum name="GL_ALWAYS"/>
--            <enum name="GL_KEEP"/>
--            <enum name="GL_REPLACE"/>
--            <enum name="GL_INCR"/>
--            <enum name="GL_DECR"/>
--            <enum name="GL_INVERT"/>
--            <enum name="GL_INCR_WRAP"/>
--            <enum name="GL_DECR_WRAP"/>
--            <enum name="GL_VENDOR"/>
--            <enum name="GL_RENDERER"/>
--            <enum name="GL_VERSION"/>
--            <enum name="GL_EXTENSIONS"/>
--            <enum name="GL_TEXTURE"/>
--            <enum name="GL_TEXTURE_CUBE_MAP"/>
--            <enum name="GL_TEXTURE_BINDING_CUBE_MAP"/>
--            <enum name="GL_MAX_CUBE_MAP_TEXTURE_SIZE"/>
--            <enum name="GL_TEXTURE0"/>
--            <enum name="GL_TEXTURE1"/>
--            <enum name="GL_TEXTURE2"/>
--            <enum name="GL_TEXTURE3"/>
--            <enum name="GL_TEXTURE4"/>
--            <enum name="GL_TEXTURE5"/>
--            <enum name="GL_TEXTURE6"/>
--            <enum name="GL_TEXTURE7"/>
--            <enum name="GL_TEXTURE8"/>
--            <enum name="GL_TEXTURE9"/>
--            <enum name="GL_TEXTURE10"/>
--            <enum name="GL_TEXTURE11"/>
--            <enum name="GL_TEXTURE12"/>
--            <enum name="GL_TEXTURE13"/>
--            <enum name="GL_TEXTURE14"/>
--            <enum name="GL_TEXTURE15"/>
--            <enum name="GL_TEXTURE16"/>
--            <enum name="GL_TEXTURE17"/>
--            <enum name="GL_TEXTURE18"/>
--            <enum name="GL_TEXTURE19"/>
--            <enum name="GL_TEXTURE20"/>
--            <enum name="GL_TEXTURE21"/>
--            <enum name="GL_TEXTURE22"/>
--            <enum name="GL_TEXTURE23"/>
--            <enum name="GL_TEXTURE24"/>
--            <enum name="GL_TEXTURE25"/>
--            <enum name="GL_TEXTURE26"/>
--            <enum name="GL_TEXTURE27"/>
--            <enum name="GL_TEXTURE28"/>
--            <enum name="GL_TEXTURE29"/>
--            <enum name="GL_TEXTURE30"/>
--            <enum name="GL_TEXTURE31"/>
--            <enum name="GL_ACTIVE_TEXTURE"/>
--            <enum name="GL_FLOAT_VEC2"/>
--            <enum name="GL_FLOAT_VEC3"/>
--            <enum name="GL_FLOAT_VEC4"/>
--            <enum name="GL_INT_VEC2"/>
--            <enum name="GL_INT_VEC3"/>
--            <enum name="GL_INT_VEC4"/>
--            <enum name="GL_BOOL"/>
--            <enum name="GL_BOOL_VEC2"/>
--            <enum name="GL_BOOL_VEC3"/>
--            <enum name="GL_BOOL_VEC4"/>
--            <enum name="GL_FLOAT_MAT2"/>
--            <enum name="GL_FLOAT_MAT3"/>
--            <enum name="GL_FLOAT_MAT4"/>
--            <enum name="GL_SAMPLER_2D"/>
--            <enum name="GL_SAMPLER_CUBE"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_ENABLED"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_SIZE"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_STRIDE"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_TYPE"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_NORMALIZED"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_POINTER"/>
--            <enum name="GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING"/>
--            <enum name="GL_IMPLEMENTATION_COLOR_READ_TYPE"/>
--            <enum name="GL_IMPLEMENTATION_COLOR_READ_FORMAT"/>
--            <enum name="GL_COMPILE_STATUS"/>
--            <enum name="GL_INFO_LOG_LENGTH"/>
--            <enum name="GL_SHADER_SOURCE_LENGTH"/>
--            <enum name="GL_SHADER_COMPILER"/>
--            <enum name="GL_SHADER_BINARY_FORMATS"/>
--            <enum name="GL_NUM_SHADER_BINARY_FORMATS"/>
--            <enum name="GL_LOW_FLOAT"/>
--            <enum name="GL_MEDIUM_FLOAT"/>
--            <enum name="GL_HIGH_FLOAT"/>
--            <enum name="GL_LOW_INT"/>
--            <enum name="GL_MEDIUM_INT"/>
--            <enum name="GL_HIGH_INT"/>
--            <enum name="GL_FRAMEBUFFER"/>
--            <enum name="GL_RENDERBUFFER"/>
--            <enum name="GL_RENDERBUFFER_WIDTH"/>
--            <enum name="GL_RENDERBUFFER_HEIGHT"/>
--            <enum name="GL_RENDERBUFFER_INTERNAL_FORMAT"/>
--            <enum name="GL_RENDERBUFFER_RED_SIZE"/>
--            <enum name="GL_RENDERBUFFER_GREEN_SIZE"/>
--            <enum name="GL_RENDERBUFFER_BLUE_SIZE"/>
--            <enum name="GL_RENDERBUFFER_ALPHA_SIZE"/>
--            <enum name="GL_RENDERBUFFER_DEPTH_SIZE"/>
--            <enum name="GL_RENDERBUFFER_STENCIL_SIZE"/>
--            <enum name="GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE"/>
--            <enum name="GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME"/>
--            <enum name="GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL"/>
--            <enum name="GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE"/>
--            <enum name="GL_NONE"/>
--            <enum name="GL_FRAMEBUFFER_COMPLETE"/>
--            <enum name="GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT"/>
--            <enum name="GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT"/>
--            <enum name="GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS"/>
--            <enum name="GL_FRAMEBUFFER_UNSUPPORTED"/>
--            <enum name="GL_FRAMEBUFFER_BINDING"/>
--            <enum name="GL_RENDERBUFFER_BINDING"/>
--            <enum name="GL_MAX_RENDERBUFFER_SIZE"/>
--            <enum name="GL_INVALID_FRAMEBUFFER_OPERATION"/>

   subtype Clear_Buffer_Mask_Bits is GLbitfield
     with Static_Predicate =>
       Clear_Buffer_Mask_Bits in GL_COLOR_BUFFER_BIT
                                   | GL_DEPTH_BUFFER_BIT
                                   | GL_STENCIL_BUFFER_BIT;

   type Clear_Buffer_Mask is private;

   function "+" (Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask;
   function "+"
    (Left  : Clear_Buffer_Mask_Bits;
     Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask;
   function "+"
    (Left  : Clear_Buffer_Mask;
     Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask;

   function Is_Set
    (Mask : Clear_Buffer_Mask; Bit : Clear_Buffer_Mask_Bits) return Boolean;

   type Buffer_Type is (Vertex, Index);

   type Shader_Type is (Vertex, Fragment);

   type Texture_Type is
    (Texture_2D,
     Cube_Map_Positive_X,
     Cube_Map_Negative_X,
     Cube_Map_Positive_Y,
     Cube_Map_Negative_Y,
     Cube_Map_Positive_Z,
     Cube_Map_Negative_Z);

   type Uniform_Location is private;

   No_Uniform_Location : constant Uniform_Location;

   type Attribute_Location is
     new Interfaces.Integer_32 range -1 .. Interfaces.Integer_32'Last;

   No_Attribute_Location : constant Attribute_Location := -1;

private

   type GLbitfield is new Interfaces.Unsigned_32;

   type GLenum is new Interfaces.Unsigned_32;

   type Clear_Buffer_Mask is new GLbitfield;

   type GLfixed is new Interfaces.Integer_32;

   type Uniform_Location is
     new WebAPI.WebGL.Uniform_Locations.WebGL_Uniform_Location_Access;

   No_Uniform_Location : constant Uniform_Location := null;

   GL_DEPTH_BUFFER_BIT   : constant GLbitfield := 16#0000_0100#;
   GL_STENCIL_BUFFER_BIT : constant GLbitfield := 16#0000_0400#;
   GL_COLOR_BUFFER_BIT   : constant GLbitfield := 16#0000_4000#;

   GL_ZERO                        : constant GLenum := 16#0000#;
   GL_ONE                         : constant GLenum := 16#0001#;

   GL_POINTS                      : constant GLenum := 16#0000#;
   GL_LINES                       : constant GLenum := 16#0001#;
   GL_LINE_LOOP                   : constant GLenum := 16#0002#;
   GL_LINE_STRIP                  : constant GLenum := 16#0003#;
   GL_TRIANGLES                   : constant GLenum := 16#0004#;
   GL_TRIANGLE_STRIP              : constant GLenum := 16#0005#;
   GL_TRIANGLE_FAN                : constant GLenum := 16#0006#;

   GL_SRC_COLOR                   : constant GLenum := 16#0300#;
   GL_ONE_MINUS_SRC_COLOR         : constant GLenum := 16#0301#;
   GL_SRC_ALPHA                   : constant GLenum := 16#0302#;
   GL_ONE_MINUS_SRC_ALPHA         : constant GLenum := 16#0303#;
   GL_DST_ALPHA                   : constant GLenum := 16#0304#;
   GL_ONE_MINUS_DST_ALPHA         : constant GLenum := 16#0305#;
   GL_DST_COLOR                   : constant GLenum := 16#0306#;
   GL_ONE_MINUS_DST_COLOR         : constant GLenum := 16#0307#;
   GL_SRC_ALPHA_SATURATE          : constant GLenum := 16#0308#;

   GL_CULL_FACE                   : constant GLenum := 16#0B44#;
   GL_DEPTH_TEST                  : constant GLenum := 16#0B71#;
   GL_STENCIL_TEST                : constant GLenum := 16#0B90#;
   GL_DITHER                      : constant GLenum := 16#0BD0#;
   GL_BLEND                       : constant GLenum := 16#0BE2#;

   GL_SCISSOR_TEST                : constant GLenum := 16#0C11#;

   GL_TEXTURE_2D                  : constant GLenum := 16#0DE1#;

   GL_BYTE                        : constant GLenum := 16#1400#;
   GL_UNSIGNED_BYTE               : constant GLenum := 16#1401#;
   GL_SHORT                       : constant GLenum := 16#1402#;
   GL_UNSIGNED_SHORT              : constant GLenum := 16#1403#;
   GL_FLOAT                       : constant GLenum := 16#1406#;
   GL_FIXED                       : constant GLenum := 16#140C#;

   GL_ALPHA                       : constant GLenum := 16#1906#;
   GL_RGB                         : constant GLenum := 16#1907#;
   GL_RGBA                        : constant GLenum := 16#1908#;
   GL_LUMINANCE                   : constant GLenum := 16#1909#;
   GL_LUMINANCE_ALPHA             : constant GLenum := 16#190A#;

   GL_NEAREST                     : constant GLenum := 16#2600#;
   GL_LINEAR                      : constant GLenum := 16#2601#;

   GL_NEAREST_MIPMAP_NEAREST      : constant GLenum := 16#2700#;
   GL_LINEAR_MIPMAP_NEAREST       : constant GLenum := 16#2701#;
   GL_NEAREST_MIPMAP_LINEAR       : constant GLenum := 16#2702#;
   GL_LINEAR_MIPMAP_LINEAR        : constant GLenum := 16#2703#;

   GL_TEXTURE_MAG_FILTER          : constant GLenum := 16#2800#;
   GL_TEXTURE_MIN_FILTER          : constant GLenum := 16#2801#;
   GL_TEXTURE_WRAP_S              : constant GLenum := 16#2802#;
   GL_TEXTURE_WRAP_T              : constant GLenum := 16#2803#;

   GL_REPEAT                      : constant GLenum := 16#2901#;

   GL_CONSTANT_COLOR              : constant GLenum := 16#8001#;
   GL_ONE_MINUS_CONSTANT_COLOR    : constant GLenum := 16#8002#;
   GL_CONSTANT_ALPHA              : constant GLenum := 16#8003#;
   GL_ONE_MINUS_CONSTANT_ALPHA    : constant GLenum := 16#8004#;
   GL_UNSIGNED_SHORT_4_4_4_4      : constant GLenum := 16#8033#;
   GL_UNSIGNED_SHORT_5_5_5_1      : constant GLenum := 16#8034#;
   GL_POLYGON_OFFSET_FILL         : constant GLenum := 16#8037#;
   GL_RGBA4                       : constant GLenum := 16#8056#;
   GL_RGB5_A1                     : constant GLenum := 16#8057#;
   GL_SAMPLE_ALPHA_TO_COVERAGE    : constant GLenum := 16#809E#;
   GL_SAMPLE_COVERAGE             : constant GLenum := 16#80A0#;

   GL_CLAMP_TO_EDGE               : constant GLenum := 16#812F#;
   GL_DEPTH_COMPONENT16           : constant GLenum := 16#81A5#;

   GL_UNSIGNED_SHORT_5_6_5        : constant GLenum := 16#8363#;
   GL_MIRRORED_REPEAT             : constant GLenum := 16#8370#;

   GL_TEXTURE_CUBE_MAP_POSITIVE_X : constant GLenum := 16#8515#;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X : constant GLenum := 16#8516#;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y : constant GLenum := 16#8517#;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y : constant GLenum := 16#8518#;
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z : constant GLenum := 16#8519#;
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z : constant GLenum := 16#851A#;

   GL_COLOR_ATTACHMENT0           : constant GLenum := 16#8CE0#;

   GL_DEPTH_ATTACHMENT            : constant GLenum := 16#8D00#;
   GL_STENCIL_ATTACHMENT          : constant GLenum := 16#8D20#;
   GL_STENCIL_INDEX8              : constant GLenum := 16#8D48#;
   GL_RGB565                      : constant GLenum := 16#8D62#;

end OpenGL;
