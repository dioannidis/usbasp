/* Name: usb_descriptors.h
 *
 * 2018 WCID support by Marius Greuel ( https://github.com/mariusgreuel )
 * 2022 Composite WCID and HID by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *
 * Tabsize: 4
 * License: GNU GPL v2 (see Readme.txt)
 */

#ifndef __usb_descriptors_h_included__
#define __usb_descriptors_h_included__

#define USBDESCR_IAD                                0x0B

#define REQUEST_GET_MS_1_0_EXTEND_PROPERTIES        0xC1

#define MS_1_0_OS_DESCRIPTOR_INDEX                  0xEE
#define MS_1_0_VENDOR_CODE                          0x5D        /* Can be anything (0 - 255) */

#define MS_1_0_GENRE_FEATURE_INDEX                  0x01
#define MS_1_0_EXTEND_COMPAT_ID_FEATURE_INDEX       0x04
#define MS_1_0_EXTEND_PROPERTIES_FEATURE_INDEX      0x05

/* USB device descriptor */
PROGMEM const char usbDescriptorDevice[] = {
    0x12,                                                   /* sizeof(usbDescriptorDevice): length of descriptor in bytes */
    USBDESCR_DEVICE,                                        /* descriptor type */
    0x00, 0x02,                                             /* USB version supported */
    0x00,
    0x00,
    0x00,                                                   /* protocol */
    8,                                                      /* max packet size */
    /* the following two casts affect the first byte of the constant only, but
    *  that's sufficient to avoid a warning with the default values.
    */
    (char)USB_CFG_VENDOR_ID,                                /* 2 bytes */
    (char)USB_CFG_DEVICE_ID,                                /* 2 bytes */
    USB_CFG_DEVICE_VERSION,                                 /* 2 bytes */
    1,                                                      /* manufacturer string index */
    2,                                                      /* product string index */
    3,                                                      /* serial number string index */
    1,                                                      /* number of configurations */
};

PROGMEM const char usbDescriptorConfiguration[] = {
    9,                                                      /* sizeof(usbDescrConfig): length of descriptor in bytes */
    USBDESCR_CONFIG,                                        /* descriptor type */
    0x32,
    0,                                                      /* total length of data returned (including inlined descriptors) */
    2,                                                      /* number of interfaces in this configuration */
    1,                                                      /* index of this configuration */
    0,                                                      /* configuration name string index */
    #if USB_CFG_IS_SELF_POWERED
    (1 << 7) | USBATTR_SELFPOWER,                           /* attributes */
    #else
    (1 << 7),                                               /* attributes */
    #endif
    USB_CFG_MAX_BUS_POWER/2,                                /* max USB current in 2mA units */

    /* interface descriptor follows inline: */

    9,                                                      /* sizeof(usbDescrInterface): length of descriptor in bytes */
    USBDESCR_INTERFACE,                                     /* descriptor type */
    0,                                                      /* index of this interface */
    0,                                                      /* alternate setting for this interface */
    0,                                                      /* endpoints excl 0: number of endpoint descriptors to follow */
    0xFF,                                                   /* USB_CFG_INTERFACE_CLASS */
    0,                                                      /* USB_CFG_INTERFACE_SUBCLASS */
    0,                                                      /* USB_CFG_INTERFACE_PROTOCOL */
    2,                                                      /* string index for interface */

    9,                                                      /* sizeof(usbDescrInterface): length of descriptor in bytes */
    USBDESCR_INTERFACE,                                     /* descriptor type */
    1,                                                      /* index of this interface */
    0,                                                      /* alternate setting for this interface */
    2,                                                      /* endpoints excl 0: number of endpoint descriptors to follow */
    0x03,                                                   /* USB_CFG_INTERFACE_CLASS */
    0,                                                      /* USB_CFG_INTERFACE_SUBCLASS */
    0,                                                      /* USB_CFG_INTERFACE_PROTOCOL */
    2,                                                      /* string index for interface */

    9,                                                      /* sizeof(usbDescrInterface): length of descriptor in bytes */
    USBDESCR_HID,                                           /* descriptor type */
    0x01, 0x01,                                             /* BCD representation of HID version */
    0x00,                                                   /* target country code */
    0x01,                                                   /* number of HID Report (or other HID class) Descriptor infos to follow */
    0x22,                                                   /* descriptor type: report */
    (USB_CFG_HID_REPORT_DESCRIPTOR_LENGTH & 0xFF),          /* descriptor length (low byte) */
    ((USB_CFG_HID_REPORT_DESCRIPTOR_LENGTH >> 8) & 0xFF),   /*            (high byte) */

    7,                                                      /* sizeof(usbDescrEndpoint) */
    USBDESCR_ENDPOINT,                                      /* descriptor type = endpoint */
    (char)0x81,                                             /* IN endpoint number 1 */
    0x03,                                                   /* attrib: Interrupt endpoint */
    8, 0,                                                   /* maximum packet size */
    USB_CFG_INTR_POLL_INTERVAL, /* in ms */

    7,                                                      /* sizeof(usbDescrEndpoint) */
    USBDESCR_ENDPOINT,                                      /* descriptor type = endpoint */
    (char)0x01,                                             /* OUT endpoint number 1 */
    0x03,                                                   /* attrib: Interrupt endpoint */
    8, 0,                                                   /* maximum packet size */
    USB_CFG_INTR_POLL_INTERVAL, /* in ms */

};

/* Microsft OS 1.0 String Desriptor */
PROGMEM const char MS_1_0_OS_STRING_DESCRIPTOR[18] = {
    0x12,                                                    /* Length: An unsigned byte and MUST be set to 0x14. */
    0x00,                                                    /* Type: An unsigned byte and MUST be set to 0x03. */
    'M', 0, 'S', 0, 'F', 0, 'T', 0, '1', 0, '0', 0, '0', 0,  /* Signature: A Unicode string and MUST be set to "MSFT100". */
    MS_1_0_VENDOR_CODE,                                      /* MS Vendor Code: An unsigned byte,
                                                                 it will be used to retrieve associated feature descriptors. */
    0x00                                                     /* Pad: An unsigned byte and MUST be set to 0x00. */
};

/* Microsft OS 1.0 Extended Compatible ID feature descriptor */
PROGMEM const char MS_1_0_OS_EXTENDED_COMPAT_ID_FEATURE[] = {
    /* Header */
    0x40, 0x00, 0x00, 0x00,                                 /* OS Extended Compatible ID feature descriptor length */
    0x00, 0x01,                                             /* OS Extended Compatible ID version */
    0x04, 0x00,                                             /* Index */
    0x02,                                                   /* Configurations count */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,               /* Reserved */
    /* Configuration WinUSB */
    0x00,                                                   /* First Interface Number */
    0x01,                                                   /* Reserved */
    'W','I','N','U','S','B', 0x00, 0x00,                    /* Windows string Compatible ID */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,         /* Windows string SubCompatible ID */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00,                     /* Reserved */
    /* Configuration HID */
    0x01,                                                   /* Second Interface Number */
    0x01,                                                   /* Reserved */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,         /* Windows string Compatible ID */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,         /* Windows string SubCompatible ID */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00                      /* Reserved */
};

/* Microsft OS 1.0 Extended Compatible ID feature descriptor */
PROGMEM const char MS_1_0_OS_EXTENDED_PROPERTIES_FEATURE_INTF0[] = {
    /* Header */
    0x8e, 0x00, 0x00, 0x00,                                 /* OS Extended Compatible ID feature descriptor length */
    0x00, 0x01,                                             /* OS Extended Compatible ID version */
    0x05, 0x00,                                             /* Index */
    0x01, 0x00,                                             /* Configurations count */
    0x84, 0x00, 0x00, 0x00, 
    0x01, 0x00, 0x00, 0x00, 
    0x28, 0x00, 
    'D',0x00,'e',0x00,'v',0x00,'i',0x00,'c',0x00,
    'e',0x00,'I',0x00,'n',0x00,'t',0x00,'e',0x00,
    'r',0x00,'f',0x00,'a',0x00,'c',0x00,'e',0x00,
    'G',0x00,'U',0x00,'I',0x00,'D',0x00,0x00,0x00,      
    0x4e, 0x00, 0x00, 0x00,                                                 
    '{',0x00,'C',0x00,'C',0x00,'4',0x00,'B',0x00,
        'C',0x00,'0',0x00,'3',0x00,'1',0x00,'-',0x00,
        'F',0x00,'6',0x00,'1',0x00,'A',0x00,'-',0x00,
        '4',0x00,'C',0x00,'2',0x00,'B',0x00,'-',0x00,
        'B',0x00,'B',0x00,'B',0x00,'2',0x00,'-',0x00,
        '7',0x00,'9',0x00,'5',0x00,'8',0x00,'7',0x00,
        '2',0x00,'B',0x00,'3',0x00,'8',0x00,'F',0x00,
        '6',0x00,'6',0x00,'}',0x00,0x00,0x00
};

/* USB HID Report descriptor */
PROGMEM const char usbDescriptorHidReport[] = {
    0x06, 0x00, 0xFF,               // USAGE_PAGE (Vendor Defined Page 1)
    0x09, 0x01,                     // USAGE (Vendor Usage 1)
    0xA1, 0x01,                     // COLLECTION (Application) 
    0x75, 0x08,                     //   REPORT_SIZE (8) 
    0x15, 0x01,                     //   LOGICAL_MINIMUM (0) 
    0x26, 0xFF, 0x00,               //   LOGICAL_MAXIMUM (255) 
    0x95, 0x08,                     //   REPORT_COUNT (8) 
    0x09, 0x01,                     //   USAGE (Vendor Usage 1) 
    0x81, 0x02,                     //   INPUT (Data,Var,Abs) 
    0x95, 0x08,                     //   REPORT_COUNT (8) 
    0x09, 0x01,                     //   USAGE (Vendor Usage 1) 
    0x91, 0x02,                     //   OUTPUT (Data,Var,Abs) 
    0x95, 0x08,                     //   REPORT_COUNT (8) 
    0x09, 0x01,                     //   USAGE (Vendor Usage 1) 
    0xB2, 0x02, 0x01,               //   FEATURE (Data,Var,Abs,Buf)
    0xC0                            // END_COLLECTION
};

#endif