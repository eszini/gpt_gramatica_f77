module finnucfl_decs
    implicit none
    INTEGER (kind=2) :: NUM_OF_ASSET_CLASSES=0,MAX_ASSET_CLASS_NUM=0
    integer (kind=2), allocatable :: ASSET_CLASS_POINTER(:)

      
    real, allocatable :: tnnfv(:,:), tddb(:,:),tdbamr(:,:)
    real, allocatable :: trbddb(:,:)
    real, allocatable :: tnfrb(:,:),tnfip(:,:),trbfip(:,:)
    real, allocatable :: tnfce(:,:),tafc1c(:,:),tnfis(:,:)
    real, allocatable :: tafc2c(:,:),tnfes(:,:),tafdc2(:,:)
    real, allocatable :: tafdc1(:,:),texexp(:,:),ttxexp(:,:)
    real, allocatable :: tafexp(:,:),tafcaj(:,:),twodft(:,:)
    real, allocatable :: tafdcb(:,:)
    real, allocatable :: tafdcd(:,:),tnfce_leased(:,:)
    real, allocatable :: tpcapinrst(:,:)
    real, allocatable :: jdbamr(:),jpcapinrst(:),jtxexp(:)
    real, allocatable :: rbcwip_afdc_meth2(:),rb_afdc1(:)
    real, allocatable :: owned_nf_tax_dep(:,:)
    real, allocatable :: owned_nf_sl_tax_depreciation(:,:)
    real, allocatable :: leasor_nf_tax_dep(:,:)
    real, allocatable :: leasor_nf_deferred_tax_basis(:,:)
    real, allocatable :: leasor_nf_sl_tax_depreciation(:,:)
    real, allocatable :: leasor_nf_book_depreciation(:,:)


    REAL, allocatable, save :: CWIP(:),NFIS(:),NNFV(:),TAXEXP(:), &
        NFCE(:),NFES(:),AFDC1(:), &
          AFDC1C(:),AFDC2(:),RBCWIP(:),DDB(:),RBDDB(:),AFCEXP(:), &
          AJAFDC(:),EXEXP(:),DBAMRT(:),NFRB(:),AFDC2C(:),WODFTX(:), &
          AFDC1B(:),AFDCDF(:),CAPINRST(:),PCAPINRST(:), &
          NF_TAX_DEP(:), &
          NF_DEFERRED_TAX_BASIS(:), &
          NF_BOOK_DEPRECIATION(:)

end module finnucfl_decs