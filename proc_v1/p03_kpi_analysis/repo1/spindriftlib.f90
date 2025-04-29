module SpinDriftLib

interface  LOCATEW
      subroutine LOCATEW_444(WindowHandle,Row,Col)
        integer (kind=4) :: WindowHandle,Row,Col
      end

      subroutine LOCATEW_442(WindowHandle,Row,Col)
        integer (kind=4) :: WindowHandle,Row
        integer (kind=2) :: Col
      end

      subroutine LOCATEW_424(WindowHandle,Row,Col)
        integer (kind=4) :: WindowHandle,Col
        integer (kind=2) :: Row      
      end

      subroutine LOCATEW_422(WindowHandle,Row,Col)
        integer (kind=4) :: WindowHandle
        integer (kind=2) :: Row,Col 
      end

      subroutine LOCATEW_244(WindowHandle,Row,Col)
        integer (kind=4) :: Row,Col
        integer (kind=2) :: WindowHandle
      end

      subroutine LOCATEW_242(WindowHandle,Row,Col)
        integer (kind=4) :: Row
        integer (kind=2) :: WindowHandle,Col
      end

      subroutine LOCATEW_224(WindowHandle,Row,Col)
        integer (kind=4) :: Col
        integer (kind=2) :: WindowHandle,Row
      end

      subroutine LOCATEW_222(WindowHandle,Row,Col)
        integer (kind=2) :: WindowHandle,Row,Col
      end
  end interface


interface locate
  
      subroutine locate_22(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer (kind=2) :: Row,Col
      end
      subroutine locate_24(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer (kind=2) :: Row
        integer (kind=4) :: Col
      end
      subroutine locate_42(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer (kind=4) :: Row
        integer (kind=2) :: Col
      end
      subroutine locate_44(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
        integer (kind=4) :: Row,Col
      end
end interface

interface cls
  subroutine cls(ULRow,ULColumn,LRRow,LRColumn)
      integer, optional :: ULRow,ULColumn,LRRow,LRColumn
  end subroutine cls
end interface


end module SpinDriftLib