#' 模具预览
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_preview()
MouldResumeServer_preview <- function(input,output,session,dms_token) {

  file_MouldResume=tsui::var_file('file_MouldResume')


  shiny::observeEvent(input$btn_MouldResume_pre,{

    filename=file_MouldResume()
    if(is.null(filename)){
      tsui::pop_notice('请上传需要预览的文件')
    }
    else{

      data <- readxl::read_excel(filename,
col_types =  c("text","text","text","text","text","text","numeric","text","date","numeric","date","date"))

     # data=mdlMouldResumePkg::MouldResume_preview(token = dms_token,file_name = filename)

      data = as.data.frame(data)
      data = tsdo::na_standard(data)


      tsui::run_dataTable2(id = 'dt_MouldResume',data = data)
    }
  })

}




#' 模具上传
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_upload()
MouldResumeServer_upload <- function(input,output,session,dms_token) {

  file_MouldResume=tsui::var_file('file_MouldResume')
  shiny::observeEvent(input$btn_MouldResume_upload,{

    filename=file_MouldResume()
    if(is.null(filename)){
      tsui::pop_notice('请选择需要上传的文件')
    }
    else{
      mdlMouldResumePkg::MouldResume_upload(token = dms_token,file_name = filename)

      tsui::pop_notice("上传成功")

    }
  })
}

#' 同步erp数据到中台
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_get()
MouldResumeServer_get <- function(input, output, session, dms_token,erp_token) {

  text_MouldResume_erp_fyear = tsui::var_text('text_MouldResume_erp_fyear')

  text_MouldResume_erp_fmonth = tsui::var_text('text_MouldResume_erp_fmonth')

  shiny::observeEvent(input$btn_MouldResume_product_get, {

    FYEAR=text_MouldResume_erp_fyear()

    FMONTH=text_MouldResume_erp_fmonth()


    mdlMouldResumePkg::productdaily_erpsync(erp_token =erp_token ,dms_token = dms_token,FYEAR = FYEAR,FMONTH =FMONTH )

  })
}


#' 预处理数据查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_preget()
MouldResumeServer_preget <- function(input, output, session, dms_token) {

  text_MouldResume_dms_fyear = tsui::var_text('text_MouldResume_dms_fyear')

  text_MouldResume_dms_fmonth = tsui::var_text('text_MouldResume_dms_fmonth')


  shiny::observeEvent(input$btn_MouldResume_get, {



    FYEAR=text_MouldResume_dms_fyear()
    FMONTH=text_MouldResume_dms_fmonth()


    data = mdlMouldResumePkg::mdlMouldResume_previewall(dms_token =dms_token ,FYEAR =FYEAR ,FMONTH = FMONTH)
    tsui::run_dataTable2(id = 'dt_MouldResume',data = data)
    tsui::run_download_xlsx(id = 'btn_MouldResume_get_download',data = data,filename = '模具履历生产数据.xlsx')

  })
}


#' 模具信息查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_mould()
MouldResumeServer_mould <- function(input, output, session, dms_token) {

  text_MouldResume_mould_fbillno = tsui::var_text('text_MouldResume_mould_fbillno')

  text_MouldResume_mould_fbillno_delete = tsui::var_text('text_MouldResume_mould_fbillno_delete')


  shiny::observeEvent(input$btn_MouldResume_mould_fbillno_view, {
    MouldNumber=text_MouldResume_mould_fbillno()

    data = mdlMouldResumePkg::mdlMouldResume_mould_viewall(dms_token =dms_token ,MouldNumber = MouldNumber)
    tsui::run_dataTable2(id = 'dt_MouldResume',data = data)

  })


  shiny::observeEvent(input$btn_MouldResume_mould_fbillno_delete, {
    MouldNumber=text_MouldResume_mould_fbillno_delete()

    data = mdlMouldResumePkg::mdlMouldResume_mould_delete(dms_token =dms_token ,MouldNumber = MouldNumber)

    tsui::pop_notice("删除成功")

  })
}


#' 按模具编号查询模具履历
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_view()
MouldResumeServer_view <- function(input, output, session, dms_token) {

  text_MouldResume_FMoldNumber = tsui::var_text('text_MouldResume_FMoldNumber')

  shiny::observeEvent(input$btn_MouldResume_view, {
    FMoldNumber = text_MouldResume_FMoldNumber()

    if (FMoldNumber=='') {
      data=mdlMouldResumePkg::mdlMouldResume_viewall(token = dms_token)
    }else{
      data = mdlMouldResumePkg::mdlMouldResume_view(token = dms_token, FMoldNumber =FMoldNumber )


    }

      tsui::run_dataTable2(id = 'dt_MouldResume', data = data)
      tsui::run_download_xlsx(id = 'btn_download',data = data,filename = '博宇翔鹰模具履历.xlsx')

  })
}




#' 按模具编号和出库日期删除模具履历
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_delete()
MouldResumeServer_delete <- function(input,output,session,dms_token) {

  text_MouldResume_FMoldNumber_delete=tsui::var_text('text_MouldResume_FMoldNumber_delete')
  text_MouldResume_FOutboundDate_delete=tsui::var_date('text_MouldResume_FOutboundDate_delete')

  shiny::observeEvent(input$btn_MouldResume_delete,{
    FMoldNumber_delete=text_MouldResume_FMoldNumber_delete()
    FOutboundDate_delete=text_MouldResume_FOutboundDate_delete()

    if (is.null(FMoldNumber_delete) || is.null(FOutboundDate_delete)) {
      tsui::pop_notice("输入参数不能为空")
      return()
    }

    result <- tryCatch({
      mdlMouldResumePkg::MouldResume_delete(token = dms_token,FMoldNumber = FMoldNumber_delete, FOutboundDate = FOutboundDate_delete)
      TRUE
    }, error = function(e) {
      tsui::pop_notice("删除失败")
    })

    if (result) {
      tsui::pop_notice("删除成功")
    }
  })
}

#' 上传模具履历
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer_update_upload()
MouldResumeServer_update_upload <- function(input,output,session,dms_token) {


  file_MouldResume_update=tsui::var_file('file_MouldResume_update')

  shiny::observeEvent(input$btn_MouldResume_preview,{
    print('点击模具履历预览')

    filename=file_MouldResume_update()
    if(is.null(filename)){
      tsui::pop_notice('请选择需要上传的文件')
    }
    else{
      data =mdlMouldResumePkg::MouldResume_update_preview(token =dms_token,file_name = filename )

      tsui::run_dataTable2(id = 'dt_MouldResume',data = data)

    }
  })

  shiny::observeEvent(input$btn_MouldResume_update_upload,{

    filename=file_MouldResume_update()
    if(is.null(filename)){
      tsui::pop_notice('请选择需要上传的文件')
    }
    else{
      mdlMouldResumePkg::MouldResume_update_upload(dms_token = dms_token,file_name = filename)

      tsui::pop_notice("上传成功")

    }
  })




}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' MouldResumeServer()
MouldResumeServer <- function(input,output,session,dms_token) {


  MouldResumeServer_preview(input=input,output=output,session=session,dms_token=dms_token)
  MouldResumeServer_upload(input=input,output=output,session=session,dms_token=dms_token)
  MouldResumeServer_view(input=input,output=output,session=session,dms_token=dms_token)
  MouldResumeServer_delete(input=input,output=output,session=session,dms_token=dms_token)
  MouldResumeServer_get(input=input,output=output,session=session,dms_token=dms_token,erp_token)
  MouldResumeServer_preget(input=input,output=output,session=session,dms_token=dms_token)
  MouldResumeServer_update_upload(input=input,output=output,session=session,dms_token=dms_token)
  MouldResumeServer_mould(input=input,output=output,session=session,dms_token=dms_token)
}
