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
with System.Storage_Elements;

with League.Strings;

private with WebAPI.WebGL.Programs;
private with WebAPI.WebGL.Rendering_Contexts;

with OpenGL.Shaders;

package OpenGL.Programs is

   pragma Preelaborate;

   type OpenGL_Program is tagged limited private;

   type OpenGL_Program_Access is access all OpenGL_Program'Class;

   function Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : League.Strings.Universal_String) return Boolean;
   --  Compiles source as a shader of the specified type and adds it to this
   --  shader program. Returns True if compilation was successful, False
   --  otherwise. The compilation errors and warnings will be made available
   --  via Log.

   procedure Add_Shader_From_Source_Code
    (Self        : in out OpenGL_Program'Class;
     Shader_Type : OpenGL.Shader_Type;
     Source      : League.Strings.Universal_String);
   --  Compiles source as a shader of the specified type and adds it to this
   --  shader program. Raise Program_Error if compilation was not successful.
   --  The compilation errors and warnings will be made available via Log.

   function Attribute_Location
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String) return OpenGL.Attribute_Location;
   --  Returns the location of the attribute Name within this shader program's
   --  parameter list. Returns -1 if name is not a valid attribute for this
   --  shader program.

   function Bind (Self : in out OpenGL_Program'Class) return Boolean;
   --  Binds this shader program to the active OpenGL_Context and makes it the
   --  current shader program. Any previously bound shader program is released.
   --  This is equivalent to calling glUseProgram().

   procedure Bind (Self : in out OpenGL_Program'Class);
   --  Binds this shader program to the active OpenGL_Context and makes it the
   --  current shader program. Any previously bound shader program is released.
   --  This is equivalent to calling glUseProgram().

   function Create (Self : in out OpenGL_Program'Class) return Boolean;
   --  Requests the shader program's id to be created immediately. Returns true
   --  if successful; false otherwise.

   procedure Disable_Attribute_Array
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location);
   --  Disables the vertex array at Location in this shader program that was
   --  enabled by a previous call to Enable_Attribute_Array.

   procedure Disable_Attribute_Array
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String);
   --  Disables the vertex array called name in this shader program that was
   --  enabled by a previous call to Enable_Attribute_Array.

   procedure Enable_Attribute_Array
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location);
   --  Enables the vertex array at Location in this shader program so that the
   --  value set by Set_Attribute_Array on Location will be used by the shader
   --  program.

   procedure Enable_Attribute_Array
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String);
   --  Enables the vertex array called Name in this shader program so that the
   --  value set by Set_Attribute_Array on Name will be used by the shader
   --  program.

   function Is_Linked (Self : OpenGL_Program'Class) return Boolean;
   --  Returns true if this shader program has been linked; false otherwise.

   not overriding function Link (Self : in out OpenGL_Program) return Boolean;
   --  Links together the shaders that were added to this program with
   --  Add_Shader. Returns True if the link was successful or False otherwise.
   --  If the link failed, the error messages can be retrieved with Log.

   procedure Release (Self : OpenGL_Program'Class);
   --  Releases the active shader program from the current OpenGL_Context. This
   --  is equivalent to calling glUseProgram(0).

   procedure Set_Attribute_Buffer
    (Self       : in out OpenGL_Program'Class;
     Location   : OpenGL.Attribute_Location;
     Data_Type  : OpenGL.GLenum;
     Tuple_Size : Positive;
     Offset     : System.Storage_Elements.Storage_Count := 0;
     Stride     : System.Storage_Elements.Storage_Count := 0;
     Normalized : Boolean                               := True);
   --  Sets an array of vertex values on the attribute at Location in this
   --  shader program, starting at a specific Offset in the currently bound
   --  vertex buffer. The Stride indicates the number of bytes between
   --  vertices. A default Stride value of zero indicates that the vertices are
   --  densely packed in the value array.
   --
   --  The Data_Type indicates the type of elements in the vertex value array,
   --  usually GL_FLOAT, GL_UNSIGNED_BYTE, etc. The Tuple_Size indicates the
   --  number of components per vertex: 1, 2, 3, or 4.
   --
   --  The array will become active when Enable_Attribute_Array is called on
   --  the Location. Otherwise the value specified with Set_Attribute_Value for
   --  Location will be used.

   procedure Set_Attribute_Buffer
    (Self       : in out OpenGL_Program'Class;
     Name       : League.Strings.Universal_String;
     Data_Type  : OpenGL.GLenum;
     Tuple_Size : Positive;
     Offset     : System.Storage_Elements.Storage_Count := 0;
     Stride     : System.Storage_Elements.Storage_Count := 0;
     Normalized : Boolean                               := True);
   --  Sets an array of vertex values on the attribute called Name in this
   --  shader program, starting at a specific Offset in the currently bound
   --  vertex buffer. The Stride indicates the number of bytes between
   --  vertices. A default stride value of zero indicates that the vertices are
   --  densely packed in the value array.
   --
   --  The Data_Type indicates the type of elements in the vertex value array,
   --  usually GL_FLOAT, GL_UNSIGNED_BYTE, etc. The Tuple_Size indicates the
   --  number of components per vertex: 1, 2, 3, or 4.
   --
   --  The array will become active when Enable_Attribute_Array is called on
   --  the Name. Otherwise the value specified with Set_Attribute_Value for
   --  Name will be used.

   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat);
   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_2);
   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_3);
   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Vector_4);
   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Matrix_2x2);
   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Matrix_3x3);
   procedure Set_Attribute_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Attribute_Location;
     Value    : OpenGL.GLfloat_Matrix_4x4);
   --  Sets the attribute at Location in the current context to Value.

   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat);
   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Vector_2);
   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Vector_3);
   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Vector_4);
   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Matrix_2x2);
   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Matrix_3x3);
   procedure Set_Attribute_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.GLfloat_Matrix_4x4);
   --  Sets the attribute called Name in the current context to Value.

   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat);
   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_2);
   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_3);
   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Vector_4);
   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_2x2);
   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_3x3);
   procedure Set_Uniform_Value
    (Self     : in out OpenGL_Program'Class;
     Location : OpenGL.Uniform_Location;
     Value    : OpenGL.Glfloat_Matrix_4x4);
   --  Sets the uniform variable at Location in the current context to Value.

   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat);
   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Vector_2);
   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Vector_3);
   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Vector_4);
   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Matrix_2x2);
   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Matrix_3x3);
   procedure Set_Uniform_Value
    (Self  : in out OpenGL_Program'Class;
     Name  : League.Strings.Universal_String;
     Value : OpenGL.Glfloat_Matrix_4x4);
   --  Sets the uniform variable called Name in the current context to Value.

   function Uniform_Location
    (Self : in out OpenGL_Program'Class;
     Name : League.Strings.Universal_String) return OpenGL.Uniform_Location;
   --  Returns the location of the uniform variable Name within this shader
   --  program's parameter list. Returns No_Uniform_Location if Name is not a
   --  valid uniform variable for this shader program.

private

   type OpenGL_Program is tagged limited record
      Program : WebAPI.WebGL.Programs.WebGL_Program_Access;
      Context : WebAPI.WebGL.Rendering_Contexts.WebGL_Rendering_Context_Access;
   end record;

end OpenGL.Programs;
