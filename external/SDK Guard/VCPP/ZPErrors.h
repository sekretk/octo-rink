//
//  Values are 32 bit values laid out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//


//
// Define the severity codes
//
#define STATUS_SEVERITY_OK               0x0
#define STATUS_SEVERITY_FAIL             0x2


//
// MessageId: ZP_S_CANCELLED
//
// MessageText:
//
// Operation canceled.
//
#define ZP_S_CANCELLED                   ((HRESULT)0x00040201L)

//
// MessageId: ZP_S_NOTFOUND
//
// MessageText:
//
// Not found.
//
#define ZP_S_NOTFOUND                    ((HRESULT)0x00040202L)

//
// MessageId: ZP_S_TIMEOUT
//
// MessageText:
//
// Operation was timed out.
//
#define ZP_S_TIMEOUT                     ((HRESULT)0x00040203L)

//
// MessageId: ZP_E_OPENNOTEXIST
//
// MessageText:
//
// Port not exists.
//
#define ZP_E_OPENNOTEXIST                ((HRESULT)0x80040203L)

//
// MessageId: ZP_E_OPENPORT
//
// MessageText:
//
// Error opening port.
//
#define ZP_E_OPENPORT                    ((HRESULT)0x80040205L)

//
// MessageId: ZP_E_PORTIO
//
// MessageText:
//
// Communication error.
//
#define ZP_E_PORTIO                      ((HRESULT)0x80040206L)

//
// MessageId: ZP_E_PORTSETUP
//
// MessageText:
//
// Error configuring port.
//
#define ZP_E_PORTSETUP                   ((HRESULT)0x80040207L)

//
// MessageId: ZP_E_LOADFTD2XX
//
// MessageText:
//
// Failed to load FTD2XX.DLL.
//
#define ZP_E_LOADFTD2XX                  ((HRESULT)0x80040208L)

//
// MessageId: ZP_E_SOCKET
//
// MessageText:
//
// Socket Error.
//
#define ZP_E_SOCKET                      ((HRESULT)0x80040209L)

//
// MessageId: ZP_E_SERVERCLOSE
//
// MessageText:
//
// Object is closed from the outside.
//
#define ZP_E_SERVERCLOSE                 ((HRESULT)0x8004020AL)

//
// MessageId: ZP_E_NOTINITALIZED
//
// MessageText:
//
// "Initialize" has not been called.
//
#define ZP_E_NOTINITALIZED               ((HRESULT)0x8004020BL)

//
// MessageId: ZP_E_INSUFFICIENTBUFFER
//
// MessageText:
//
// The buffer size is too small.
//
#define ZP_E_INSUFFICIENTBUFFER          ((HRESULT)0x8004020CL)

//
// MessageId: ZP_E_NOCONNECT
//
// MessageText:
//
// There is no connection.
//
#define ZP_E_NOCONNECT                   ((HRESULT)0x8004020DL)

