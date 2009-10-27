;
; NAME:
; 	DICOM_WRITER
;
; VERSION:
;	0.21
;
; PURPOSE:
;	Generate a dicom file from within RSI IDL
; 
; AUTHOR:
;	Bhautik Joshi
;
; EMAIL:
;	bjoshi@geocities.com
;
; HOMEPAGE:
;	http://cow.mooh.org
;
; USE:
;	DICOM_WRITER, filename, image, VOXELSIZE = voxelsize, SSAI = ssai, $
;		PATIENT = patient, PHYSICIAN = physician, PATID = patid
;
; INPUT:
;	filename - string containing name of dicom file to be written to
;	image - Integer (BYTE, FIX, UINT, LONG or ULONG) image - type and bpp
;	is now automagically set
;
; OPTIONAL PARAMETERS
;	voxelsize - Array of 3 floating point values representing voxel size
;	with the format [x,y,z], otherwise set to default of [1.0,1.0,1.0]
;	ssai - Array of 4 integer values representing studyID, seriesnum,
;       acqnum,imagenum, with the format [studyID,seriesnum,acqnum,imagenum],
;	otherwise set to default of [0,0,0,0]
;	patient - patient name, if not defined, set to dummy name
;	physician - physician name, if not defined, set to dummy name
;	patid - patiend ID, if not defined, set to dummy name
;
; NOTES ON USAGE (READ! IMPORTANT!):
;	* At the moment the program only writes to a single slice
;	* Extra dicom tags can be easily added (see body of program, especially
;	  generate_VRtag function)
;	* There is little to no error-checking at the moment, so be 
;	  careful!
;	* Analyse seems to need a minimum image size of somewhere around
;	  100x100
;	* IMPORTANT: The DICOM writer tries to write 'Implicit VR' type
;	  DICOM files - see DICOM standard PS 3.5-1999, part 7
;	* Can write most VR (Value Represenation) tags via new function,
;	  generate_VRtag. Currently supported:
;	  AE, AS, AT, CS, DA, DS, DT, FL, FD, FD, IS, LO, LT, OB, OW,
;	  SH, SL, SS, ST, TM, UI, UL, UN, US, UT
;	  and SQ, PN unsupported (I got away with using UI in place of PN)
;	* See comments near generate_VRtag function for notes on usage
;	  of the function for adding your own additional tags
;
; EXAMPLE:
;	Create a horrendously boring byte image and store it in a 
;	dicom file, test.dcm, with voxel dimensions of [2.0,3.0,4.0],
;	and studyid=1,series number=2,acquisiton number=3 and image
;	number=4:
;
;	> rows = 200
;	> cols = 200
;	> image = indgen(rows,cols)
;	> dicom_writer, 'test.dcm', image, voxelsize=[2.0,3.0,4.0], ssai=[1,2,3,4]
;
; HISTORY:
;	Based on Marc O'Briens (m.obrien@sghms.ac.uk) TIFF_to_DICOM.c
;	version 0.1 	08-01-2002 - first working version produced
;	version 0.11	09-01-2002 - fixed endian-ness issue & added get_lun
;				     functionality
;	version 0.2	14-01-2002 - many fixes and additions, including:
;		* replaced most generate_* functions with generate_VRtag
;		* support for many VR (Value representation) types
;		* Autodetection of little/big endian architecture and
;		  automagic byte ordering as necessary (for tags/image)
;		* automagically detect image type and set bpp as necessary
;		* more data in the header can be set manually
;	version 0.21	15-01-2002 - uploaded all over the place & fixed bug
;				     that didn't update patient, patid etc.
;
; TODO:
;	* Allow for more robust dicom writing
;	* Part 10 compliance (!!!!!!!!!!!)
;	* Decent error checking
;
; DISCLAIMER:
; 
; Permission to use, copy, modify, and distribute this software and its
; documentation for any purpose and without fee is hereby granted,
; provided that the above copyright notice appear in all copies and that
; both that copyright notice and this permission notice appear in
; supporting documentation.
;
; This file is provided AS IS with no warranties of any kind.  The author
; shall have no liability with respect to the infringement of copyrights,
; trade secrets or any patents by this file or any part thereof.  In no
; event will the author be liable for any lost revenue or profits or
; other special, indirect and consequential damages.
; 
; The author accepts no responsibility for any action arising from use of 
; this package. The software is not guaranteed to write compliant DICOM
; files. If it causes damage to you or your system, you have been warned -
; this is a work in progress. If it bites your dog, its not my fault. If
; it causes you to curl up on the floor in the foetal position muttering
; about pixies and mushrooms, its not my fault. If it causes you or someone
; else to spontaneously burst into song and dance, its not my fault but
; I'd like to hear about it. You have been warned.
;

;convert a value, val, that is num bytes long, into 
;a series of ordered bytes
function getbytes, val, num
	ret=BYTARR(num)
	offset=0
;work in big endian ONLY
	;val=swap_endian(val)
;if (!version.arch eq 'x86') then begin REPLACED with little endian test
;thanks to David Fanning :)
	little_endian = (BYTE(1, 0, 1))[0]
	if (little_endian) then begin
		byteorder,val,/SWAP_IF_BIG_ENDIAN 
	endif else begin
		byteorder,val,/SWAP_IF_LITTLE_ENDIAN 
	endelse
	for i=0,(num-1) do begin
		tmpres=BYTE(ISHFT(val, offset) AND 255)
		ret[i]=tmpres
		offset=offset-8
	endfor

	return, ret
end

;generate any tag
function generate_anytag, group, element, data, STR = str

	pad=BYTE(0)

;check to see if string type is set - if it is, change 
;padding byte to a space
	IF KEYWORD_SET(STR) then begin 
		pad=BYTE(STRING(' '))
	endif

	rs=[getbytes(group,2),getbytes(element,2)]

;correct to even length if necessary
	bs=BYTE(data)
	nl=n_elements(bs)
	if ((nl mod 2) ne 0) then begin
		bs=[bs,pad]
		nl=nl+1
	end
;size of field
	rs=[rs,getbytes(nl,2)]
;padding
	rs=[rs,[0,0]]
;string itself
	rs=[rs,bs]

	return, rs
end

; generate a tag based on its data and VR (value representation)
; based on DICOM specs 3.5-1999 (table 6.2-1)
;
; usage: generate_VRtag(group, element, 'XX', data)
;        where XX is one of the supported VR types below
;
; This is a list of the current VR types supported/not supported
; and the expected data type for the 'data' variable
;
; AE:
; * Application Entity - normal string tag
; * 16 bytes max
; * STRING
;
; AS:
; * Age String - should be nnnX, where X={D,W,M,Y} (days, weeks, months, years),
; * 4 bytes fixed
; * STRING
;
; AT:
; * Attribute tag - should be a pair of unsigned integers representing a data
;   element tag eg. ['0018'x,'00FF'x]
; * 8 bytes fixed
; * [UINT,UINT]
;
; CS:
; * Code string
; * 32 bytes maximum
; * STRING
;
; DA:
; * Date string - 8 bytes fixed, formay yyyymmdd, or 10 bytes fixed
;   yyyy.mm.dd, which is compatible with versions prior dicom v3.0 -
;   so thats what will be used
; * 10 bytes fixed
; * STRING
;
; DS
; * Decimal string - convert an float into a string, and store
; * 16 bytes maximum
; * FLOAT/DOUBLE
;
; DT
; * Date/time string - 26 byte maximum string of format:
;   YYYMMDDGGMMSS.FFFFFF
; * 26 bytes max
; * STRING
; 
; FL:
; * Floating point single - 4 byte fp single val
; * storing as LITTLE ENDIAN - needs to be checked!!!!
; * 4 bytes fixed
; * FLOAT
; 
; FD:
; * Floating point double - 8 byte fp double val
; * storing as LITTLE ENDIAN - needs to be checked!!!!
; * 8 bytes fixed
; * DOUBLE 
;
; IS:
; * Decimal string - convert an int into a string, and store
; * 12 bytes maximum
; * FIX
; 
; LO:
; * long string - IDL doesn't care about this one
; * 64 bytes maximum
; * LONG
;
; LT:
; * long text - IDL doesn't care about this one too much
; * 10240 bytes maximum
; * STRING
; 
; OB
; * other byte string - padded by 00H
; * length variable
; * STRING/BYTE
; 
; OW
; * other word string - padded by 00H. not sure if this is working
; * length variable
; * STRING/BYTE
;
; PN:
; * person name - not supported! (yet?)
; 
; SH
; * short string 
; * 16 bytes maximum
; * STRING
; 
; SL:
; * signed long int
; * 4 bytes fixed
; * LONG
;
; SQ:
; * sequence of items - not supported!
; 
; SS
; * signed short
; * 2 bytes fixed
; * FIX
; 
; ST:
; * short text 
; * 1024 bytes maximum
; * STRING
; 
; TM:
; * time - of format hhmmss.frac
; * 16 bytes maximum
; * STRING
;
; UI:
; * unique identifier
; * 64 bytes maximum
; * STRING
; 
; UL:
; * unsigned long
; * 4 bytes fixed
; * ULONG
; 
; UN:
; * unknown - do whatever you please with this one
; * variable length
; * STRING/BYTE
;
; US:
; * unsigned short
; * 2 bytes fixed
; * UINT
; 
; UT:
; * unlimited text; could be huge!
; * variable length
; * STRING
;
function generate_VRtag, group, element, VR, data
	
	CASE VR of

		'AE': begin
;Application Entity - normal string tag, truncated to 16 bytes
			dval=STRMID(data,0,16)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'AS': begin
;Age String - should be nnnX, where X={D,W,M,Y} (days, weeks, months, years),
;truncated to 4 bytes
			dval=STRMID(data,0,4)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'AT': begin
;Attribute tag - should be a pair of unsigned integers representing a data
;element tag eg. ['0018'x,'00FF'x]
			dval=[getbytes(UINT(data[0]),2), getbytes(UINT(data[1]),2)]
			rs=generate_anytag(group,element,dval)
		end

		'CS': begin
;Code string - 32 byte string
			dval=STRMID(data,0,32)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'DA': begin
;Date string - 8 bytes fixed, formay yyyymmdd, or 10 bytes fixed
;yyyy.mm.dd, which is compatible with versions prior dicom v3.0 -
;so thats what will be used
			dval=STRMID(data,0,10)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'DS': begin
;Decimal string - convert an float into a string, and store
;16 bytes maximum
			dval=STRTRIM(STRING(data),1)
			dval=STRMID(dval,0,16)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'DT': begin
;Date/time string - 26 byte maximum string of format:
;YYYMMDDGGMMSS.FFFFFF
			dval=STRMID(data,0,26)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'FL': begin
;Floating point single - 4 byte fp single val
;storing as LITTLE ENDIAN - needs to be checked!!!!
			dval=FLOAT(data)
;explicily cast to bytes
			dvaltmp=BYTE(dval,0,4)
;fix byteorder (little-endian) if necessary; word length is 16 bits (?is this correct?)
			dval=[getbytes(UINT(dvaltmp[0:1],0,1),2), $
			      getbytes(UINT(dvaltmp[2:3],0,1),2)]
			rs=generate_anytag(group,element,dval)
		end

		'FD': begin
;Floating point double - 8 byte fp double val
;storing as LITTLE ENDIAN - needs to be checked!!!!
			dval=DOUBLE(data)
;explicily cast to bytes
			dvaltmp=BYTE(dval,0,8)
;fix byteorder (little-endian) if necessary; word length is 16 bits (?is this correct?)
			dval=[getbytes(UINT(dvaltmp[0:1],0,1),2), $
			      getbytes(UINT(dvaltmp[2:3],0,1),2), $
			      getbytes(UINT(dvaltmp[4:5],0,1),2), $
			      getbytes(UINT(dvaltmp[6:7],0,1),2)]
			rs=generate_anytag(group,element,dval)
		end

		'IS': begin
;Decimal string - convert an int into a string, and store
;12 bytes maximum
			dval=STRTRIM(STRING(data),1)
			dval=STRMID(dval,0,12)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'LO': begin
;long string - IDL doesn't care about this one, 64 bytes max
			dval=STRMID(data,0,64)
			rs=generate_anytag(group,element,dval,/STR)			
		end

		'LT': begin
;long text - IDL doesn't care about this one too much, 10240 bytes max
			dval=STRMID(data,0,10240)
			rs=generate_anytag(group,element,dval,/STR)			
		end

		'OB': begin
;other byte string - padded by 00H
			dval=data
			rs=generate_anytag(group,element,dval)
		end

		'OW': begin
;other word string - padded by 00H. not sure if this is working
			dval=data
			rs=generate_anytag(group,element,dval)
		end

		'PN': begin
;person name - not supported! (yet?)
			print, 'PN currently unsupported!'
			rs=BYTE(0)
		end

		'SH': begin
;short string - 16 bytes max
			dval=STRMID(data,0,16)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'SL': begin
;signed long
			dval=getbytes(LONG(data),4)
			rs=generate_anytag(group,element,dval)
		end

		'SQ': begin
;sequence of items - not supported!
			print, 'SQ currently unsupported!'
			rs=BYTE(0)
		end

		'SS': begin
;signed short
			dval=getbytes(FIX(data),2)
			rs=generate_anytag(group,element,dval)
		end

		'ST': begin
;short text - 1024 bytes max
			dval=STRMID(data,0,1024)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'TM': begin
;time - of format hhmmss.frac, 16 bytes maximum
			dval=STRMID(data,0,16)
			rs=generate_anytag(group,element,dval,/STR)
		end

		'UI': begin
;unique identifier, 64 bytes maximum
			dval=STRMID(data,0,64)
			rs=generate_anytag(group,element,dval)
		end

		'UL': begin
;unsigned long
			dval=getbytes(ULONG(data),4)
			rs=generate_anytag(group,element,dval)
		end

		'UN': begin
;unknown - do whatever you please with this one
			dval=data
			rs=generate_anytag(group,element,dval)
		end

		'US': begin
;unsigned short
			dval=getbytes(UINT(data),2)
			rs=generate_anytag(group,element,dval)
		end

		'UT': begin
;unlimited text; could be huge!
			dval=data
			rs=generate_anytag(group,element,dval)
		end

	ENDCASE
	return, BYTE(rs)
end

;generate pixel tag
function generate_pixeltag, group, element, val
	return, BYTE([getbytes(group,2),getbytes(element,2), getbytes(val,4)])
end

pro dicom_writer, filename, image, VOXELSIZE = voxelsize, SSAI = ssai, $
	PATIENT = patient, PHYSICIAN = physician, PATID = patid


	print, '-------------------------------------------'
	print, 'IDL DICOM writer    by   Bhautik Joshi 2002
	print, 'http://cow.mooh.org   bjoshi@geocities.com
	print, '-----------------v 0.2---------------------'

;determine type of image and set bpp

	bpp=0

	imtype=size(image,/type)

	case imtype of
		1: bpp=1 ;byte
		2: bpp=2 ;int
		3: bpp=4 ;long int
		12: bpp=2 ;unsigned int
		13: bpp=4 ;unsigned long int
		else: bpp=0
	endcase

	if (bpp eq 0) then begin
		print, 'Only integer type images (byte/int/long) supported at this time, sorry!'
		return
	endif

	if (bpp eq 1) then print, 'Byte type (bpp=1) image'
	if (bpp eq 2) then print, 'Integer type (bpp=2) image'
	if (bpp eq 4) then print, 'Long type (bpp=4) image'

	byteswapped_image=image

	if (bpp ge 2) then begin
		little_endian = (BYTE(1, 0, 1))[0]
		if (little_endian ne 1) then begin
			print, 'Big endian architecture detected'
			byteorder,byteswapped_image,/SWAP_IF_BIG_ENDIAN 
		endif else begin
			print, 'Little endian architecture detected'
			byteorder,byteswapped_image,/SWAP_IF_LITTLE_ENDIAN 
		endelse
	endif

;dummy fill-in variables. 
	random= '123456'
;SOP class set to MR - see Annex A in PS 3.6-2001
	SOPClass = '1.2.840.10008.5.1.4.1.1.20'
	SOPInstance = '1.2.840.10008.5.1.4.1.1.20.1'
	StudyInstanceUID = SOPInstance + random
	SeriesInstanceUID = StudyInstanceUID
	RelFrameOfReferenceUID = StudyInstanceUID
	SeriesInstanceUID = SeriesInstanceUID + '.1'
	RelFrameOfReferenceUID = RelFrameOfReferenceUID + '.2'

;image variables
	sz=size(image)
	rows=sz[1]
	cols=sz[2]
	print, 'Image size = [',STRTRIM(rows,1),',',STRTRIM(cols,1),']'

;parameter set variables

;voxelsize=[x,y,z]
	if (KEYWORD_SET(voxelsize)) then begin
		thickness=STRTRIM(STRING(voxelsize[2]),1)
		spacing=STRTRIM(STRING(voxelsize[0]),1) + '\' + STRTRIM(STRING(voxelsize[1]),1)
	endif else begin
		thickness=1.0
		spacing='1.0\\1.0'
	endelse

	print, 'Slice thickness = ',STRTRIM(thickness,1)
	print, 'Spacing set at ',STRTRIM(spacing,1)

;ssai=[studyID,seriesnum,acqnum,imagenum]
	if (KEYWORD_SET(ssai)) then begin
		StudyID = ssai[0]
		Seriesnum = ssai[1]
		Acqnum = ssai[2]
		Imagenum = ssai[3]
	endif else begin
		StudyID = 0
		Seriesnum = 0
		Acqnum = 0
		Imagenum = 0
	endelse
	
	print, 'StudyID = ',STRTRIM(studyID,1),' Series# = ',STRTRIM(seriesnum,1),$
		' Acq# = ',STRTRIM(acqnum,1),' Img#= ',STRTRIM(imagenum,1)

	if (KEYWORD_SET(patient)) then patient=STRING(patient) else patient='Jabba the Hutt'
	if (KEYWORD_SET(physician)) then physician=STRING(physician) else physician='Chewbacca'
	if (KEYWORD_SET(patid)) then patid=STRING(patid) else patid='TK247'

	print, 'Patient = ', patient
	print, 'Physician = ', physician

	GET_LUN, U

	OPENW, U, filename

	print, 'Writing to file ', filename

; DICOM tags - feel free to add more!

;0008 tags

;MR type	
	WRITEU, U, generate_VRtag('0008'x,'0008'x,'CS','ORIGINAL\\PRIMARY\\OTHER')
;Instance date 
	WRITEU, U, generate_VRtag('0008'x,'0012'x,'DA','2002.01.14')
;Instance time
	WRITEU, U, generate_VRtag('0008'x,'0013'x,'TM','150101')
;SOP class
	WRITEU, U, generate_VRtag('0008'x,'0016'x,'UI',SOPClass)
;SOP instance
	WRITEU, U, generate_VRtag('0008'x,'0018'x,'UI',SOPInstance)
;Modality
	WRITEU, U, generate_VRtag('0008'x,'0060'x,'CS','MR')
;Manufacturer
	WRITEU, U, generate_VRtag('0008'x,'0070'x,'LO','GE')
;Study Physicians Name
	WRITEU, U, generate_VRtag('0008'x,'0090'x,'UI',physician)

;0010 tags

;Patient name
	WRITEU, U, generate_VRtag('0010'x,'0010'x,'UI',patient)
;Patient ID
	WRITEU, U, generate_VRtag('0010'x,'0020'x,'LO',patid)
;Patient birth date
	WRITEU, U, generate_VRtag('0010'x,'0030'x,'DA','20020114')
;Patient sex
	WRITEU, U, generate_VRtag('0010'x,'0040'x,'CS','M')

;0018 tags

;Acquisition type
	WRITEU, U, generate_VRtag('0018'x,'0023'x,'CS','2D')
;Slice thickness
	WRITEU, U, generate_VRtag('0018'x,'0050'x,'DS',thickness)

;0020 tags

;Study instance
	WRITEU, U, generate_VRtag('0020'x,'000D'x,'UI',StudyInstanceUID)
;Series instance UID
	WRITEU, U, generate_VRtag('0020'x,'000E'x,'UI',SeriesInstanceUID)
;StudyID
	WRITEU, U, generate_VRtag('0020'x,'0010'x,'IS',StudyID)
;Series number
	WRITEU, U, generate_VRtag('0020'x,'0011'x,'IS',seriesnum)
;Acquisition number
	WRITEU, U, generate_VRtag('0020'x,'0012'x,'IS',acqnum)
;Image number
	WRITEU, U, generate_VRtag('0020'x,'0013'x,'IS',imagenum)

;0028 tags

;samples per pixel
	WRITEU, U, generate_VRtag('0028'x,'0002'x,'US',1)
;Photometric interpretation
	WRITEU, U, generate_VRtag('0028'x,'0004'x,'CS','MONOCHROME2')
;Rows in image
	WRITEU, U, generate_VRtag('0028'x,'0010'x,'US',rows)
;Columns in image
	WRITEU, U, generate_VRtag('0028'x,'0011'x,'US',cols)
;pixel spacing
	WRITEU, U, generate_VRtag('0028'x,'0030'x,'DS',spacing)
;bits allocated per sample
	WRITEU, U, generate_VRtag('0028'x,'0100'x,'US',bpp*8)
;bits stored per sample
	WRITEU, U, generate_VRtag('0028'x,'0101'x,'US',bpp*8)
;high bit
	WRITEU, U, generate_VRtag('0028'x,'0102'x,'US',(bpp*8)-1)
;pixel representation
	WRITEU, U, generate_VRtag('0028'x,'0103'x,'US','0000'x)

;write image data
	imsize=rows*cols*bpp

	WRITEU, U, generate_pixeltag('7FE0'x,'0010'x,imsize)

	WRITEU, U, byteswapped_image

	CLOSE, U
	FREE_LUN, U

	print, 'Successfully wrote ', filename
end
