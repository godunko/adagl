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

package body OpenGL is

   ---------
   -- "+" --
   ---------

   function "+" (Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask is
   begin
      return Clear_Buffer_Mask (Right);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
    (Left  : Clear_Buffer_Mask_Bits;
     Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask is
   begin
      return Clear_Buffer_Mask (Left) or Clear_Buffer_Mask (Right);
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
    (Left  : Clear_Buffer_Mask;
     Right : Clear_Buffer_Mask_Bits) return Clear_Buffer_Mask is
   begin
      return Left or Clear_Buffer_Mask (Right);
   end "+";

   ------------
   -- Is_Set --
   ------------

   function Is_Set
    (Mask : Clear_Buffer_Mask; Bit : Clear_Buffer_Mask_Bits) return Boolean is
   begin
      return (Mask and Clear_Buffer_Mask (Bit)) /= 0;
   end Is_Set;

end OpenGL;
