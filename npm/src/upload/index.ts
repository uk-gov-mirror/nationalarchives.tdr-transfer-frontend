import S3 from "aws-sdk/clients/s3"
import { getAuthenticatedUploadObject, IAuthenticatedUpload } from "../auth"

export interface ITdrFile {
  fileId: string
  file: File
}

const uploadSingleFile: (
  authenticatedUploadObject: IAuthenticatedUpload,
  tdrFile: ITdrFile,
  callback: (percentageProgress: number) => void,
  totalLoaded: number,
  totalSize: number
) => Promise<S3.ManagedUpload.SendData> = (
  authenticatedUploadObject,
  tdrFile,
  callback,
  totalLoaded,
  totalSize
) => {
  const { s3, identityId } = authenticatedUploadObject
  const { file, fileId } = tdrFile
  const progress: S3.ManagedUpload = s3.upload({
    Key: `${identityId}/${fileId}`,
    Body: file,
    Bucket: "tdr-upload-files-intg"
  })
  progress.on("httpUploadProgress", ev => {
    callback(Math.round(((ev.loaded + totalLoaded) / totalSize) * 100))
  })
  return progress.promise()
}

export const uploadToS3: (
  files: ITdrFile[],
  token: string,
  callback: (percentageProgress: number) => void,
  chunkSize?: number
) => Promise<S3.ManagedUpload.SendData[]> = async (
  files,
  token,
  callback,
  chunkSize = 5 * 1024 * 1024
) => {
  const authenticatedUploadObject: IAuthenticatedUpload = await getAuthenticatedUploadObject(
    token
  )
  const size: number = files
    .map(file => file.file.size)
    .reduce((prev, curr) => prev + curr)
  let totalLoaded = 0
  const sendData: S3.ManagedUpload.SendData[] = []
  for (const file of files) {
    const uploadResult = await uploadSingleFile(
      authenticatedUploadObject,
      file,
      callback,
      totalLoaded,
      size
    )
    sendData.push(uploadResult)
    totalLoaded += file.file.size / chunkSize
  }
  return sendData
}
