module pkind
public
!private:: one_dpi; 
integer(8),parameter,private:: one_dpi=1
integer,parameter:: dpi=kind(one_dpi)
integer,parameter:: sp=kind(1.0)
integer,parameter:: dp=kind(1.0d0)
integer,parameter:: spc=kind((1.0,1.0))
integer,parameter:: dpc=kind((1.0d0,1.0d0))
end module pkind
