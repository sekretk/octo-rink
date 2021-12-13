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
// Not initialized function "Initialize".
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

//
// MessageId: ZG_E_TOOLARGEMSG
//
// MessageText:
//
// Message is too large to send.
//
#define ZG_E_TOOLARGEMSG                 ((HRESULT)0x80040301L)

//
// MessageId: ZG_E_NOANSWER
//
// MessageText:
//
// No Answer.
//
#define ZG_E_NOANSWER                    ((HRESULT)0x80040303L)

//
// MessageId: ZG_E_BADANSWER
//
// MessageText:
//
// Bad Answer.
//
#define ZG_E_BADANSWER                   ((HRESULT)0x80040304L)

//
// MessageId: ZG_E_WRONGZPORT
//
// MessageText:
//
// Not the correct version of "ZPort.dll"
//
#define ZG_E_WRONGZPORT                  ((HRESULT)0x80040305L)

//
// MessageId: ZG_E_CVTBUSY
//
// MessageText:
//
// Converter is busy.
//
#define ZG_E_CVTBUSY                     ((HRESULT)0x80040306L)

//
// MessageId: ZG_E_CVTERROR
//
// MessageText:
//
// Unknown error converter.
//
#define ZG_E_CVTERROR                    ((HRESULT)0x80040307L)

//
// MessageId: ZG_E_LICNOTFOUND
//
// MessageText:
//
// License not found.
//
#define ZG_E_LICNOTFOUND                 ((HRESULT)0x80040308L)

//
// MessageId: ZG_E_LICEXPIRED
//
// MessageText:
//
// The current license has expired.
//
#define ZG_E_LICEXPIRED                  ((HRESULT)0x80040309L)

//
// MessageId: ZG_E_LICONTROLLERS
//
// MessageText:
//
// Licensing limit controllers.
//
#define ZG_E_LICONTROLLERS               ((HRESULT)0x8004030AL)

//
// MessageId: ZG_E_LICREADKEYS
//
// MessageText:
//
// Licensing limit readable keys.
//
#define ZG_E_LICREADKEYS                 ((HRESULT)0x8004030BL)

//
// MessageId: ZG_E_LICWRITEKEYS
//
// MessageText:
//
// License restriction on the number of recordable keys.
//
#define ZG_E_LICWRITEKEYS                ((HRESULT)0x8004030CL)

//
// MessageId: ZG_E_LICEXPIRED2
//
// MessageText:
//
// License period has expired.
//
#define ZG_E_LICEXPIRED2                 ((HRESULT)0x8004030DL)

//
// MessageId: ZG_E_NOCONVERTER
//
// MessageText:
//
// Converter not found (invalid address).
//
#define ZG_E_NOCONVERTER                 ((HRESULT)0x8004030EL)

//
// MessageId: ZG_E_NOCONTROLLER
//
// MessageText:
//
// Controller not found (invalid address).
//
#define ZG_E_NOCONTROLLER                ((HRESULT)0x8004030FL)

//
// MessageId: ZG_E_CTRNACK
//
// MessageText:
//
// Controller refused to command.
//
#define ZG_E_CTRNACK                     ((HRESULT)0x80040310L)

//
// MessageId: ZG_E_FWBOOTLOADERNOSTART
//
// MessageText:
//
// Bootloader not started.
//
#define ZG_E_FWBOOTLOADERNOSTART         ((HRESULT)0x80040311L)

//
// MessageId: ZG_E_FWFILESIZE
//
// MessageText:
//
// Filesize does not match the request.
//
#define ZG_E_FWFILESIZE                  ((HRESULT)0x80040312L)

//
// MessageId: ZG_E_FWNOSTART
//
// MessageText:
//
// Not found running firmware. Try restarting the device.
//
#define ZG_E_FWNOSTART                   ((HRESULT)0x80040313L)

//
// MessageId: ZG_E_FWNOCOMPATIBLE
//
// MessageText:
//
// Not compatible for this device.
//
#define ZG_E_FWNOCOMPATIBLE              ((HRESULT)0x80040314L)

//
// MessageId: ZG_E_FWINVALIDDEVNUM
//
// MessageText:
//
// Not suitable for this device number.
//
#define ZG_E_FWINVALIDDEVNUM             ((HRESULT)0x80040315L)

//
// MessageId: ZG_E_FWTOOLARGE
//
// MessageText:
//
// Too large data firmware can be a mistake.
//
#define ZG_E_FWTOOLARGE                  ((HRESULT)0x80040316L)

//
// MessageId: ZG_E_FWSEQUENCEDATA
//
// MessageText:
//
// Violated the sequence data.
//
#define ZG_E_FWSEQUENCEDATA              ((HRESULT)0x80040317L)

//
// MessageId: ZG_E_FWDATAINTEGRITY
//
// MessageText:
//
// Compromise data integrity.
//
#define ZG_E_FWDATAINTEGRITY             ((HRESULT)0x80040318L)

