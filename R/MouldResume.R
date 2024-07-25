#' 预览
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


      data=mdlMouldResumePkg::MouldResume_preview(token = dms_token,file_name = filename)

      tsui::run_dataTable2(id = 'dt_MouldResume',data = data)
    }
  })
}


#' 上传
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



#' 按模具编号查询
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
      tsui::run_download_xlsx(id = 'btn_download',data = data,filename = '博宇翔鹰-模具履历.xlsx')

  })
}




#' 按模具编号和出库日期删除
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
      mdlMouldResumePkg::MouldResumeServer_delete(token = dms_token,FMoldNumber = FMoldNumber_delete, FOutboundDate = FOutboundDate_delete)
      TRUE
    }, error = function(e) {
      tsui::pop_notice("删除失败")
    })

    if (result) {
      tsui::pop_notice("删除成功")
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

}
