import { ClientFileProcessing } from "../src/clientprocessing"
import { UploadFiles } from "../src/upload"
import { GraphqlClient } from "../src/graphql"
import { KeycloakInstance } from "keycloak-js"

jest.mock("../src/clientprocessing")

class ClientFileProcessingSuccess {
  processFiles: (
    consignmentId: string,
    numberOfFiles: number
  ) => Promise<string[]> = async (
    consignmentId: string,
    numberOfFiles: number
  ) => {
    const data: string[] = ["1", "2"]
    return data
  }
  processClientFileMetadata: (
    files: File[],
    fileIds: string[]
  ) => Promise<void> = async (files: File[], fileIds: string[]) => {
    return Promise.resolve()
  }
}

class ClientFileProcessingError {
  processFiles: (
    consignmentId: string,
    numberOfFiles: number
  ) => Promise<number[]> = async (
    consignmentId: string,
    numberOfFiles: number
  ) => {
    return Promise.reject(Error("Process files failed"))
  }
  processClientFileMetadata: (
    files: File[],
    fileIds: string[]
  ) => Promise<void> = async (files: File[], fileIds: string[]) => {
    return Promise.reject(Error("Process client file metadata failed"))
  }
}

beforeEach(() => jest.resetModules())

const mockKeycloak: KeycloakInstance<"native"> = {
  init: jest.fn(),
  login: jest.fn(),
  logout: jest.fn(),
  register: jest.fn(),
  accountManagement: jest.fn(),
  createLoginUrl: jest.fn(),
  createLogoutUrl: jest.fn(),
  createRegisterUrl: jest.fn(),
  createAccountUrl: jest.fn(),
  isTokenExpired: jest.fn(),
  updateToken: jest.fn(),
  clearToken: jest.fn(),
  hasRealmRole: jest.fn(),
  hasResourceRole: jest.fn(),
  loadUserInfo: jest.fn(),
  loadUserProfile: jest.fn(),
  token: "fake-auth-token"
}

function setUpUpload(): UploadFiles {
  const client = new GraphqlClient("test", mockKeycloak)
  const clientFileProcessing = new ClientFileProcessing(client)

  return new UploadFiles(clientFileProcessing)
}

const mockSuccess: () => void = () => {
  const mock = ClientFileProcessing as jest.Mock
  mock.mockImplementation(() => {
    return new ClientFileProcessingSuccess()
  })
}

const mockFailure: () => void = () => {
  const mock = ClientFileProcessing as jest.Mock
  mock.mockImplementation(() => {
    return new ClientFileProcessingError()
  })
}

test("generateFileDetails returns file ids", async () => {
  mockSuccess()
  const uploadFiles = setUpUpload()

  const result = await uploadFiles.generateFileDetails("1", 2)

  expect(result).toHaveLength(2)
  expect(result[0]).toBe("1")
  expect(result[1]).toBe("2")
})

test("generateFileDetails returns an error if adding Files fails", async () => {
  mockFailure()
  const uploadFiles = setUpUpload()

  await expect(uploadFiles.generateFileDetails("1", 2)).rejects.toStrictEqual(
    Error("Process files failed")
  )
})

test("uploadClientFileMetadata adds client file metadata", async () => {
  mockSuccess()
  const uploadFiles = setUpUpload()

  await expect(
    uploadFiles.uploadClientFileMetadata([], [])
  ).resolves.not.toThrow()
})

test("uploadClientFileMetadata returns an error if uploading file metadata fails", async () => {
  mockFailure()
  const uploadFiles = setUpUpload()

  await expect(
    uploadFiles.uploadClientFileMetadata([], [])
  ).rejects.toStrictEqual(Error("Process client file metadata failed"))
})
