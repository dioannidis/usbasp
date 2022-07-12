/* Name: usb_descriptors.h
 *
 * 2018 WCID support by Marius Greuel ( https://github.com/mariusgreuel )
 * 2022 Composite WCID and HID by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 * 2022 Move to Microsoft OS 2.0 Descriptors by Dimitrios Chr. Ioannidis ( d.ioannidis@nephelae.eu )
 *
 * Tabsize: 4
 * License: GNU GPL v2 (see Readme.txt)
 */

#ifndef __usb_descriptors_h_included__
#define __usb_descriptors_h_included__

#define VENDOR_CODE                                    0x5D        /* Can be anything (0 - 255) */

#define USBDESCR_BOS                                   0x0F

#define USBDESCR_DEVICE_CAPABILITY_TYPE                0x10

#define USBDESCR_DEVICE_CAPABILITY_CONTAINER_ID        0x04
#define USBDESCR_DEVICE_CAPABILITY_PLATFORM            0x05

#define MS_OS_2_0_DESCRIPTOR_INDEX                     0x07
#define MS_OS_20_SET_ALT_ENUMERATION                   0x08

#define MS_OS_20_SET_HEADER_DESCRIPTOR                 0x00, 0x00
#define MS_OS_20_SUBSET_HEADER_CONFIGURATION           0x01, 0x00
#define MS_OS_20_SUBSET_HEADER_FUNCTION                0x02, 0x00
#define MS_OS_20_FEATURE_COMPATIBLE_ID                 0x03, 0x00
#define MS_OS_20_FEATURE_REG_PROPERTY                  0x04, 0x00
#define MS_OS_20_FEATURE_MIN_RESUME_TIME               0x05, 0x00
#define MS_OS_20_FEATURE_MODEL_ID                      0x06, 0x00
#define MS_OS_20_FEATURE_CCGP_DEVICE                   0x07, 0x00
#define MS_OS_20_FEATURE_VENDOR_REVISION               0x08, 0x00

#define MS_OS_20_REG_PROPERTY_REG_SZ                   0x01, 0x00
#define MS_OS_20_REG_PROPERTY_REG_EXPAND_SZ            0x02, 0x00
#define MS_OS_20_REG_PROPERTY_REG_BINARY               0x03, 0x00
#define MS_OS_20_REG_PROPERTY_REG_DWORD_LITLE_INDIAN   0x04, 0x00
#define MS_OS_20_REG_PROPERTY_REG_DWORD_BIG_INDIAN     0x05, 0x00
#define MS_OS_20_REG_PROPERTY_REG_LINK                 0x06, 0x00
#define MS_OS_20_REG_PROPERTY_REG_MULTI_SZ             0x07, 0x00

/* USB device descriptor */
PROGMEM const char usbDescriptorDevice[] = {
    0x12,                                                   /* sizeof(usbDescriptorDevice): length of descriptor in bytes */
    USBDESCR_DEVICE,                                        /* descriptor type */
    0x01, 0x02,                                             /* USB version supported */
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

/* BOS Descriptor */
PROGMEM const char BOS_DESCRIPTOR[] = {
    
    /* BOS Descriptor Header */
    0x05,                                                  /* Size of descriptor */
    USBDESCR_BOS,                                          /* Descriptor type */
    0x35, 0x00,                                            /* Length of this descriptor and all of its sub descriptors */
    0x02,                                                  /* The number of separate device capability descriptors in the BOS */

    /* Device Capability Descriptor - Container_ID */
    0x14,                                                  /* Size of descriptor */
    USBDESCR_DEVICE_CAPABILITY_TYPE,                       /* Descriptor Type */
    USBDESCR_DEVICE_CAPABILITY_CONTAINER_ID,               /* Device Capability Type */
    0x00,                                                  /* Reserved */
    0xB9, 0xD3, 0x57, 0xAD, 0x66, 0x11, 0xF8, 0x43,        /* UUID */
    0x87, 0x90, 0xEB, 0xE1, 0x4D, 0xDC, 0x75, 0x94,        /* {AD57D3B9-1166-43F8-8790-EBE14DDC7594} */

    /* Device Capability Descriptor - Platform */
    0x1C,                                                  /* Length */
    USBDESCR_DEVICE_CAPABILITY_TYPE,                       /* Descriptor Type */
    USBDESCR_DEVICE_CAPABILITY_PLATFORM,                   /* Device Capability Type */
    0x00,                                                  /* Reserved */
    0xDF, 0x60, 0xDD, 0xD8, 0x89, 0x45, 0xC7, 0x4C,        /* MS OS 2.0 Platform Capability */
    0x9C, 0xD2, 0x65, 0x9D, 0x9E, 0x64, 0x8A, 0x9F,        /* {D8DD60DF-4589-4CC7-9CD2-659D9E648A9F} */
    0x00, 0x00, 0x03, 0x06,                                /* Windows Version - Windows 8.1 or later */
    0xA6, 0x00,                                            /* Size of MS OS 2.0 Descriptor set */
    VENDOR_CODE,                                           /* Vendor Request Code */
    0x00                                                   /* Alternate Enumeration support - 0 No support */

};

/* Microsft OS 2.0 Descriptor Set */
PROGMEM const char MS_2_0_OS_DESCRIPTOR_SET[] = {

    /* MS OS 2.0 Descriptor Set Header */
    0x0A, 0x00,                                            /* Size of descriptor */
    MS_OS_20_SET_HEADER_DESCRIPTOR,                        /* Descriptor Type */
    0x00, 0x00, 0x03, 0x06,                                /* Windows Version - Windows 8.1 or later */
    0xA6, 0x00,                                            /* Size of MS OS 2.0 Descriptor set */
    
    /* MS OS 2.0 Function Subset Header */
    0x08, 0x00,                                            /* Size of descriptor */
    MS_OS_20_SUBSET_HEADER_FUNCTION,                       /* Descriptor Type */
    0x00,                                                  /* The interface number for the first interface of the function to which this subset applies. */
    0x00,                                                  /* Reserved */
    0x9C, 0x00,                                            /* The size of entire function subset including this header. */

    /* MS OS 2.0 Compatible ID Descriptor */
    0x14, 0x00,                                            /* Size of descriptor */
    MS_OS_20_FEATURE_COMPATIBLE_ID,                        /* Descriptor Type */
    'W','I','N','U','S','B', 0x00, 0x00,                   /* Windows string Compatible ID */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,        /* Windows string SubCompatible ID */

    /* MS OS 2.0 Registry Property Descriptor */
    0x80, 0x00,                                            /* Size of descriptor */
    MS_OS_20_FEATURE_REG_PROPERTY,                         /* Descriptor Type */
    MS_OS_20_REG_PROPERTY_REG_SZ,                          /* The type of registry property */
    0x28, 0x00,                                            /* The length of the property name */
    'D',0x00,'e',0x00,'v',0x00,'i',0x00,'c',0x00,          /* The name of the property name */
    'e',0x00,'I',0x00,'n',0x00,'t',0x00,'e',0x00,          /*    -//-    */
    'r',0x00,'f',0x00,'a',0x00,'c',0x00,'e',0x00,          /*    -//-    */
    'G',0x00,'U',0x00,'I',0x00,'D',0x00,0x00,0x00,         /*    -//-    */
    0x4e, 0x00,                                            /* The length of property data */
    '{',0x00,'A',0x00,'D',0x00,'5',0x00,'7',0x00,          /* Property data */
    'D',0x00,'3',0x00,'B',0x00,'9',0x00,'-',0x00,          /*    -//-    */
    '1',0x00,'1',0x00,'6',0x00,'6',0x00,'-',0x00,          /*    -//-    */
    '4',0x00,'3',0x00,'F',0x00,'8',0x00,'-',0x00,          /*    -//-    */
    '8',0x00,'7',0x00,'9',0x00,'0',0x00,'-',0x00,          /*    -//-    */
    '0',0x00,'B',0x00,'E',0x00,'1',0x00,'4',0x00,          /*    -//-    */
    'D',0x00,'D',0x00,'C',0x00,'7',0x00,'5',0x00,          /*    -//-    */
    '0',0x00,'4',0x00,'}',0x00,0x00,0x00                   /*    -//-    */

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