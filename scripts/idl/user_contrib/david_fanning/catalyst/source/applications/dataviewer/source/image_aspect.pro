;+========================================================================
;******************************************************************************************;
;  Copyright (c) 2013, Regents of the University of Colorado. All rights reserved.    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      1. Redistributions of source code must retain the above copyright                   ;
;         notice, this list of conditions and the following disclaimer.                    ;
;      2. Redistributions in binary form must reproduce the above copyright                ;
;         notice, this list of conditions and the following disclaimer in the              ;
;         documentation and/or other materials provided with the distribution.             ;
;      3. Neither the name of the Regents of the University of Colorado nor the names      ;
;         of its contributors may be used to endorse or promote products derived from      ;
;          this software without specific prior written permission.                        ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY THE REGENTS OF THE UNIVERISTY OF COLORADO ''AS IS'' AND    ;
;  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED           ;
;  WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.     ;
;  IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF COLORADO BE LIABLE FOR ANY DIRECT,   ;
;  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT      ;
;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR      ;
;  PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,        ;
;  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      ;
;  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE              ;
;  POSSIBILITY OF SUCH DAMAGE.                                                             ;
;******************************************************************************************;
;
; Created 01/16/2013
; National Snow & Ice Data Center, University of Colorado, Boulder
;-========================================================================*/

;+
;  computes the aspect ratio when an input true color image is input. This
;  will work with band or pixel or row interleaved images.
;
; :Params:
;    image : in, required, type="3-d array"
;       3 dimensional image object
;
;
; :Returns: the height/width of a band, pixel or row interleaved image.
;
;-
function aspect_of_true_color_image,  image
   compile_opt idl2, logical_predicate
   image_dims = size( image,  /dimensions )
   interleaved_plane = where( image_dims eq 3,  count,  $
                              complement = hw_idx, $
                              ncomplement = num_heightwidth )

   if count ne 1 then return, !Null
   if num_heightwidth ne 2 then return,  !Null

   return,  float( image_dims[ hw_idx[ 1 ] ] ) / image_dims[ hw_idx[ 0 ] ]
end

;+
;  compute an image's aspect ratio regardless of the number of planes.
;
; :Params:
;    image : in, required, type=image
;       This is the image who's aspect ratio you wish to return.  This
;       will work with a 2-D(x,y), or true color pixel/row/band interleaved
;       image
;
; :Returns: fractional aspect ratio. (height/width)
;
;-
function image_aspect,  image
   compile_opt idl2, logical_predicate

   num_dims =  size( image, /n_dimensions )
   case num_dims of
      2: begin
         the_dims =  size( image,  /dimensions )
         return, float( the_dims[ 1 ] ) / the_dims[ 0 ]
      end
      3: return, aspect_of_true_color_image( image )
      else: return,  !Null
   endcase

   ;; default
   return,  !Null
end
