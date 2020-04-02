#[macro_use]
extern crate nom;

use nom::number::streaming::{be_u16, le_u8, le_u16, le_u32};

mod cp437;

#[derive(Debug, PartialEq)]
pub struct Header {
	pub vendor: [char; 3],
	pub product: u16,
	pub serial: u32,
	pub week: u8,
	pub year: u8, // Starting at year 1990
	pub version: u8,
	pub revision: u8,
}

fn parse_vendor(v: u16) -> [char; 3] {
	let mask: u8 = 0x1F; // Each letter is 5 bits
	let i0 = ('A' as u8) - 1; // 0x01 = A
	return [
		(((v >> 10) as u8 & mask) + i0) as char,
		(((v >> 5) as u8 & mask) + i0) as char,
		(((v >> 0) as u8 & mask) + i0) as char,
	]
}

named!(parse_header<&[u8], Header>, do_parse!(
	tag!(&[0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00][..])
	>> vendor: be_u16
	>> product: le_u16
	>> serial: le_u32
	>> week: le_u8
	>> year: le_u8
	>> version: le_u8
	>> revision: le_u8
	>> (Header{vendor: parse_vendor(vendor), product, serial, week, year, version, revision})
));

#[derive(Debug, PartialEq)]
pub struct Display {
	pub video_input: u8,
	pub width: u8, // cm
	pub height: u8, // cm
	pub gamma: u8, // datavalue = (gamma*100)-100 (range 1.00–3.54)
	pub features: u8,
}

named!(parse_display<&[u8], Display>, do_parse!(
	video_input: le_u8
	>> width: le_u8
	>> height: le_u8
	>> gamma: le_u8
	>> features: le_u8
	>> (Display{video_input, width, height, gamma, features})
));

named!(parse_chromaticity<&[u8], ()>, do_parse!(
	take!(10) >> ()
));

named!(parse_established_timing<&[u8], ()>, do_parse!(
	take!(3) >> ()
));

named!(parse_standard_timing<&[u8], ()>, do_parse!(
	take!(16) >> ()
));

named!(parse_descriptor_text<&[u8], String>,
	map!(
		map!(take!(13), |b| {
			b.iter()
			.filter(|c| **c != 0x0A)
			.map(|b| cp437::forward(*b))
			.collect::<String>()
		}),
		|s| s.trim().to_string()
	)
);

#[derive(Debug, PartialEq)]
pub struct DetailedTiming {
	/// Pixel clock in kHz.
	pub pixel_clock: u32,
	pub horizontal_active_pixels: u16,
	pub horizontal_blanking_pixels: u16,
	pub vertical_active_lines: u16,
	pub vertical_blanking_lines: u16,
	pub horizontal_front_porch: u16,
	pub horizontal_sync_width: u16,
	pub vertical_front_porch: u16,
	pub vertical_sync_width: u16,
	/// Horizontal size in millimeters
	pub horizontal_size: u16,
	/// Vertical size in millimeters
	pub vertical_size: u16,
	/// Border pixels on one side of screen (i.e. total number is twice this)
	pub horizontal_border_pixels: u8,
	/// Border pixels on one side of screen (i.e. total number is twice this)
	pub vertical_border_pixels: u8,
	pub features: u8, /* TODO add enums etc. */
}

named!(parse_detailed_timing<&[u8], DetailedTiming>, do_parse!(
	pixel_clock_10khz: le_u16
	>> horizontal_active_lo: le_u8
	>> horizontal_blanking_lo: le_u8
	>> horizontal_px_hi: le_u8
	>> vertical_active_lo: le_u8
	>> vertical_blanking_lo: le_u8
	>> vertical_px_hi: le_u8
	>> horizontal_front_porch_lo: le_u8
	>> horizontal_sync_width_lo: le_u8
	>> vertical_lo: le_u8
	>> porch_sync_hi: le_u8
	>> horizontal_size_lo: le_u8
	>> vertical_size_lo: le_u8
	>> size_hi: le_u8
	>> horizontal_border: le_u8
	>> vertical_border: le_u8
	>> features: le_u8
	>> (DetailedTiming {
		pixel_clock: pixel_clock_10khz as u32 * 10,
		horizontal_active_pixels: (horizontal_active_lo as u16) |
		                          (((horizontal_px_hi >> 4) as u16) << 8),
		horizontal_blanking_pixels: (horizontal_blanking_lo as u16) |
		                            (((horizontal_px_hi & 0xf) as u16) << 8),
		vertical_active_lines: (vertical_active_lo as u16) |
		                       (((vertical_px_hi >> 4) as u16) << 8),
		vertical_blanking_lines: (vertical_blanking_lo as u16) |
		                         (((vertical_px_hi & 0xf) as u16) << 8),
		horizontal_front_porch: (horizontal_front_porch_lo as u16) |
		                        (((porch_sync_hi >> 6) as u16) << 8),
		horizontal_sync_width: (horizontal_sync_width_lo as u16) |
		                       ((((porch_sync_hi >> 4) & 0x3) as u16) << 8),
		vertical_front_porch: ((vertical_lo >> 4) as u16) |
		                      ((((porch_sync_hi >> 2) & 0x3) as u16) << 8),
		vertical_sync_width: ((vertical_lo & 0xf) as u16) |
		                     (((porch_sync_hi & 0x3) as u16) << 8),
		horizontal_size: (horizontal_size_lo as u16) | (((size_hi >> 4) as u16) << 8),
		vertical_size: (vertical_size_lo as u16) | (((size_hi & 0xf) as u16) << 8),
		horizontal_border_pixels: horizontal_border,
		vertical_border_pixels: vertical_border,
		features: features
	})
));

#[derive(Debug, PartialEq)]
pub enum Descriptor {
	DetailedTiming(DetailedTiming),
	SerialNumber(String),
	UnspecifiedText(String),
	RangeLimits, // TODO
	ProductName(String),
	WhitePoint, // TODO
	StandardTiming, // TODO
	ColorManagement,
	TimingCodes,
	EstablishedTimings,
	Dummy,
	Unknown(Vec<u8>),
}

named!(parse_descriptor<&[u8], Descriptor>,
	switch!(peek!(le_u16),
		0 => do_parse!(
			take!(3)
			>> d: switch!(le_u8,
				0xFF => do_parse!(
					take!(1)
					>> s: parse_descriptor_text
					>> (Descriptor::SerialNumber(s))
				) |
				0xFE => do_parse!(
					take!(1)
					>> s: parse_descriptor_text
					>> (Descriptor::UnspecifiedText(s))
				) |
				0xFD => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::RangeLimits)
				) |
				0xFC => do_parse!(
					take!(1)
					>> s: parse_descriptor_text
					>> (Descriptor::ProductName(s))
				) |
				0xFB => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::WhitePoint)
				) |
				0xFA => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::StandardTiming)
				) |
				0xF9 => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::ColorManagement)
				) |
				0xF8 => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::TimingCodes)
				) |
				0xF7 => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::EstablishedTimings)
				) |
				0x10 => do_parse!(
					take!(1)
					>> take!(13)
					>> (Descriptor::Dummy)
				) |
				_ => do_parse!(
					take!(1)
					>> data: count!(le_u8, 13)
					>> (Descriptor::Unknown(data))
				)
			)
			>> (d)
		) |
		_ => do_parse!(
			d: parse_detailed_timing
			>> (Descriptor::DetailedTiming(d))
		)
	)
);

#[derive(Debug, PartialEq)]
pub struct DataBlockHeader {
	pub header: u8,
}

impl DataBlockHeader {
	pub fn type_tag(&self) -> u8 {
		return self.header & 0xe0u8;
	}

	pub fn len(&self) -> usize {
		return (self.header & 0x1fu8) as usize;
	}
}

#[derive(Debug, PartialEq)]
pub struct ShortAudioDescriptor {
	pub format_and_channels: u8,
	pub sampling_frequences: u8,
	pub bitrate_or_bitdepth: u8,
}

#[allow(dead_code)]
impl ShortAudioDescriptor {
	pub fn channels(&self) -> u8 {
		return (self.format_and_channels & 0x7u8) + 1;
	}

	pub fn format(&self) -> u8 {
		return self.format_and_channels & 0x78u8;
	}

	pub fn bitrate(&self) -> Option<u32> {
		match self.format() {
			ShortAudioDescriptor::AC3..=ShortAudioDescriptor::WMAPRO => {
				Some(8000 * self.bitrate_or_bitdepth as u32)
			}
			_ => None,
		}
	}

	pub fn bit_depths(&self) -> Option<u8> {
		match self.format() {
			ShortAudioDescriptor::LPCM => Some(self.bitrate_or_bitdepth & 0x7u8),
			_ => None,
		}
	}

	// audio format values
	pub const LPCM: u8 = 1u8 << 3;
	pub const AC3: u8 = 2u8 << 3;
	pub const MPEG1: u8 = 3u8 << 3;
	pub const MP3: u8 = 4u8 << 3;
	pub const MPEG2: u8 = 5u8 << 3;
	pub const AAC: u8 = 6u8 << 3;
	pub const DTS: u8 = 7u8 << 3;
	pub const ATRAC: u8 = 8u8 << 3;
	pub const DSD: u8 = 9u8 << 3;
	pub const DDPLUS: u8 = 10u8 << 3;
	pub const DTSHD: u8 = 11u8 << 3;
	pub const TRUEHD: u8 = 12u8 << 3;
	pub const DSTAUDIO: u8 = 13u8 << 3;
	pub const WMAPRO: u8 = 14u8 << 3;
	pub const RESERVED: u8 = 15u8 << 3;

	// supported sampling frequences
	pub const SAMPLE_FREQUENCY_192_KHZ: u8 = 1u8 << 6;
	pub const SAMPLE_FREQUENCY_176_KHZ: u8 = 1u8 << 5;
	pub const SAMPLE_FREQUENCY_96_KHZ: u8 = 1u8 << 4;
	pub const SAMPLE_FREQUENCY_88_KHZ: u8 = 1u8 << 3;
	pub const SAMPLE_FREQUENCY_48_KHZ: u8 = 1u8 << 2;
	pub const SAMPLE_FREQUENCY_44_1_KHZ: u8 = 1u8 << 1;
	pub const SAMPLE_FREQUENCY_32_KHZ: u8 = 1u8 << 0;

	// supported bid depth (if audio format is LPCM)
	pub const LPCM_24_BIT: u8 = 1u8 << 2;
	pub const LPCM_20_BIT: u8 = 1u8 << 1;
	pub const LPCM_16_BIT: u8 = 1u8 << 0;
}

#[derive(Debug, PartialEq)]
pub struct AudioBlock {
	pub header: DataBlockHeader,
	pub descriptors: Vec<ShortAudioDescriptor>,
}

#[derive(Debug, PartialEq)]
pub struct ShortVideoDescriptor {
	pub payload: u8,
}

#[allow(dead_code)]
impl ShortVideoDescriptor {
	pub fn is_native(&self) -> bool {
		(self.payload & 0x80u8) != 0 && (self.payload & 0x7fu8) <= 64
	}

	pub fn cea861_index(&self) -> usize {
		if self.payload <= (64u8 | 0x80u8) {
			(self.payload & !0x80u8) as usize
		} else {
			self.payload as usize
		}
	}
}

#[derive(Debug, PartialEq)]
pub struct VideoBlock {
	pub header: DataBlockHeader,
	pub descriptors: Vec<ShortVideoDescriptor>,
}

#[derive(Debug, PartialEq)]
pub struct VendorSpecific {
	pub header: DataBlockHeader,
	pub identifier: [u8; 3],
	pub payload: Vec<u8>,
}

#[derive(Debug, PartialEq)]
pub struct SpeakerAllocation {
	pub header: DataBlockHeader,
	pub speakers: u8,
	pub reserved: [u8; 2],
}

impl SpeakerAllocation {
	pub const REAR_LEFT_RIGHT_CENTER: u8 = (1u8 << 6);
	pub const FRONT_LEFT_RIGHT_CENTER: u8 = (1u8 << 5);
	pub const REAR_CENTER: u8 = (1u8 << 4);
	pub const REAR_LEFT_RIGHT: u8 = (1u8 << 3);
	pub const FRONT_CENTER: u8 = (1u8 << 2);
	pub const LFE: u8 = (1u8 << 1);
	pub const FRONT_LEFT_RIGHT: u8 = (1u8 << 0);
}

#[derive(Debug, PartialEq)]
pub struct DataBlockReserved {
	pub header: DataBlockHeader,
	pub payload: Vec<u8>,
}

#[derive(Debug, PartialEq)]
pub enum DataBlock {
	Reserved(DataBlockReserved),
	AudioBlock(AudioBlock),
	VideoBlock(VideoBlock),
	VendorSpecific(VendorSpecific),
	SpeakerAllocation(SpeakerAllocation),
}

#[derive(Debug, PartialEq)]
pub struct CEAEDID {
	pub native_dtd: u8,
	pub blocks: Vec<DataBlock>,
	pub descriptors: Vec<DetailedTiming>,
}

impl CEAEDID {
	// native DTD information bits
	pub const DTD_UNDERSCAN: u8 = (1u8 << 7); // display supports underscan
	pub const DTD_BASIC_AUDIO: u8 = (1u8 << 6); // display supports basic audio
	pub const DTD_YUV444: u8 = (1u8 << 5); // display supports YCbCr 4∶4∶4
	pub const DTD_YUV422: u8 = (1u8 << 4); // display supports YCbCr 4∶2∶2
}

fn error_tag<E>(input: &[u8]) -> nom::IResult<&[u8], E> {
	Err(nom::Err::Error((input, nom::error::ErrorKind::Tag)))
}

fn error_needed<E>(_input: &[u8], size: usize) -> nom::IResult<&[u8], E> {
	Err(nom::Err::Incomplete(nom::Needed::Size(size)))
}

fn parse_block(input: &[u8]) -> nom::IResult<&[u8], Vec<DataBlock>> {
	let mut v = Vec::new();
	let mut i = &input[..];
	while i.len() > 0 {
		let hdr = DataBlockHeader { header: i[0] };
		i = &i[1..];
		let len = hdr.len();
		if len > i.len() {
			return error_needed(i, hdr.len() - i.len());
		}
		match hdr.type_tag() {
			0x20 => {
				if len < 3 || len % 3 != 0 {
					return error_tag(i);
				}
				let mut d = Vec::new();
				let mut pos = 0;
				while pos < len {
					d.push(ShortAudioDescriptor {
						format_and_channels: i[pos],
						sampling_frequences: i[pos + 1],
						bitrate_or_bitdepth: i[pos + 2],
					});
					pos += 3;
				}
				v.push(DataBlock::AudioBlock(AudioBlock {
					header: hdr,
					descriptors: d,
				}));
			}

			0x40 => {
				if len < 1 {
					return error_tag(i);
				}
				let mut d = Vec::new();
				let mut pos = 0;
				while pos < len {
					d.push(ShortVideoDescriptor { payload: i[pos] });
					pos += 1;
				}
				v.push(DataBlock::VideoBlock(VideoBlock {
					header: hdr,
					descriptors: d,
				}));
			}

			0x60 => {
				if len < 3 {
					return error_tag(i);
				}
				v.push(DataBlock::VendorSpecific(VendorSpecific {
					header: hdr,
					identifier: [i[0], i[1], i[2]],
					payload: Vec::from(&i[3..len]),
				}));
			}

			0x80 => {
				if len != 3 {
					return error_tag(i);
				}
				if i[1] != 0 || i[2] != 0 {
					return error_tag(i);
				}
				v.push(DataBlock::SpeakerAllocation(SpeakerAllocation {
					header: hdr,
					speakers: i[0],
					reserved: [i[1], i[2]],
				}));
			}

			_ => v.push(DataBlock::Reserved(DataBlockReserved {
				header: hdr,
				payload: Vec::from(&i[..len]),
			})),
		}
		i = &i[len..];
	}
	Ok((i, v))
}

fn parse_descriptors(input: &[u8]) -> nom::IResult<&[u8], Vec<DetailedTiming>> {
	let mut v = Vec::new();
	let mut i = &input[..];
	while i.len() >= 18 && (i[0] != 0 || i[1] != 0) {
		let (rest, dtd) = parse_detailed_timing(i)?;
		v.push(dtd);
		i = &rest[..];
	}
	Ok((i, v))
}

fn parse_extension(i: &[u8]) -> nom::IResult<&[u8], CEAEDID> {
	validate(i)?;
	let offs = i[2] as usize;
	let native_dtd = i[3];
	if offs == 0 {
		return Ok((
			&i[128..],
			CEAEDID {
				native_dtd,
				blocks: Vec::new(),
				descriptors: Vec::new(),
			},
		));
	}
	let (_, blocks) = parse_block(&i[4..offs])?;
	let (mut tail, descriptors) = parse_descriptors(&i[offs..127])?;
	while tail.len() > 0 {
		if tail[0] != 0 {
			return error_tag(tail);
		}
		tail = &tail[1..];
	}
	Ok((
		&i[128..],
		CEAEDID {
			native_dtd,
			blocks,
			descriptors,
		},
	))
}

#[derive(Debug, PartialEq)]
pub struct EDID {
	pub header: Header,
	pub display: Display,
	chromaticity: (),       // TODO
	established_timing: (), // TODO
	standard_timing: (),    // TODO
	pub descriptors: Vec<Descriptor>,
	pub extension: Option<CEAEDID>,
}

fn validate(input: &[u8]) -> nom::IResult<&[u8], ()> {
	if input.len() < 128 {
		return error_needed(input, 128 - input.len());
	}

	let mut sum = 0u8;
	for b in &input[..128] {
		sum = sum.wrapping_add(*b);
	}
	if sum == 0u8 {
		Ok((input, ()))
	} else {
		error_tag(input)
	}
}

named!(parse_edid<&[u8], EDID>, do_parse!(
	call!(validate)
	>> header: parse_header
	>> display: parse_display
	>> chromaticity: parse_chromaticity
	>> established_timing: parse_established_timing
	>> standard_timing: parse_standard_timing
	>> descriptors: count!(parse_descriptor, 4)
	>> extension_nr: le_u8
	>> take!(1) // checksum
	>> extension: cond!(extension_nr == 1, parse_extension)
	>> (EDID{header, display, chromaticity, established_timing, standard_timing, descriptors, extension})
));

pub fn parse(data: &[u8]) -> nom::IResult<&[u8], EDID> {
	parse_edid(data)
}

#[cfg(test)]
mod tests {
	use super::*;

	fn test(d: &[u8], expected: &EDID) {
		match parse(d) {
			Ok((remaining, parsed)) => {
				assert_eq!(remaining.len(), 0);
				assert_eq!(&parsed, expected);
			},
			Err(err) => {
				panic!(format!("{}", err));
			},
		}
	}

	#[test]
	fn test_card0_vga_1() {
		let d = include_bytes!("../testdata/card0-VGA-1");

		let expected = EDID{
			header: Header{
				vendor: ['S', 'A', 'M'],
				product: 596,
				serial: 1146106418,
				week: 27,
				year: 17,
				version: 1,
				revision: 3,
			},
			display: Display{
				video_input: 14,
				width: 47,
				height: 30,
				gamma: 120,
				features: 42,
			},
			chromaticity: (),
			established_timing: (),
			standard_timing: (),
			descriptors: vec!(
				Descriptor::DetailedTiming(DetailedTiming {
					pixel_clock: 146250,
					horizontal_active_pixels: 1680,
					horizontal_blanking_pixels: 560,
					vertical_active_lines: 1050,
					vertical_blanking_lines: 39,
					horizontal_front_porch: 104,
					horizontal_sync_width: 176,
					vertical_front_porch: 3,
					vertical_sync_width: 6,
					horizontal_size: 474,
					vertical_size: 296,
					horizontal_border_pixels: 0,
					vertical_border_pixels: 0,
					features: 28
				}),
				Descriptor::RangeLimits,
				Descriptor::ProductName("SyncMaster".to_string()),
				Descriptor::SerialNumber("HS3P701105".to_string()),
			),
			extension: None,
		};

		test(d, &expected);
	}

	#[test]
	fn test_card0_edp_1() {
		let d = include_bytes!("../testdata/card0-eDP-1");

		let expected = EDID{
			header: Header{
				vendor: ['S', 'H', 'P'],
				product: 5193,
				serial: 0,
				week: 32,
				year: 25,
				version: 1,
				revision: 4,
			},
			display: Display{
				video_input: 165,
				width: 29,
				height: 17,
				gamma: 120,
				features: 14,
			},
			chromaticity: (),
			established_timing: (),
			standard_timing: (),
			descriptors: vec!(
				Descriptor::DetailedTiming(DetailedTiming {
					pixel_clock: 138500,
					horizontal_active_pixels: 1920,
					horizontal_blanking_pixels: 160,
					vertical_active_lines: 1080,
					vertical_blanking_lines: 31,
					horizontal_front_porch: 48,
					horizontal_sync_width: 32,
					vertical_front_porch: 3,
					vertical_sync_width: 5,
					horizontal_size: 294,
					vertical_size: 165,
					horizontal_border_pixels: 0,
					vertical_border_pixels: 0,
					features: 24,
				}),
				Descriptor::Dummy,
				Descriptor::UnspecifiedText("DJCP6ÇLQ133M1".to_string()),
				Descriptor::Unknown(vec![2, 65, 3, 40, 0, 18, 0, 0, 11, 1, 10, 32, 32]),
			),
			extension: None,
		};

		test(d, &expected);
	}

	#[test]
	fn test_card0_hdmi_1() {
		let d = include_bytes!("../testdata/card0-HDMI-1");

		let expected = EDID{
			header: Header {
				vendor: ['D', 'E', 'L'],
				product: 41099,
				serial: 809851217,
				week: 15,
				year: 23,
				version: 1,
				revision: 3,
			},
			display: Display {
				video_input: 128,
				width: 53,
				height: 30,
				gamma: 120,
				features: 234,
			},
			chromaticity: (),
			established_timing: (),
			standard_timing: (),
			descriptors: vec![
				Descriptor::DetailedTiming(DetailedTiming {
					pixel_clock: 148500,
					horizontal_active_pixels: 1920,
					horizontal_blanking_pixels: 280,
					vertical_active_lines: 1080,
					vertical_blanking_lines: 45,
					horizontal_front_porch: 88,
					horizontal_sync_width: 44,
					vertical_front_porch: 4,
					vertical_sync_width: 5,
					horizontal_size: 531,
					vertical_size: 299,
					horizontal_border_pixels: 0,
					vertical_border_pixels: 0,
					features: 30,
				}),
				Descriptor::SerialNumber("67Y4J34A0EYQ".to_string()),
				Descriptor::ProductName("DELL S2440L".to_string()),
				Descriptor::RangeLimits,
			],
			extension: Some(CEAEDID {
				native_dtd: 241,
				blocks: vec![
					DataBlock::VideoBlock(VideoBlock {
						header: DataBlockHeader { header: 76 },
						descriptors: vec![
							ShortVideoDescriptor { payload: 144 },
							ShortVideoDescriptor { payload: 5 },
							ShortVideoDescriptor { payload: 4 },
							ShortVideoDescriptor { payload: 3 },
							ShortVideoDescriptor { payload: 2 },
							ShortVideoDescriptor { payload: 7 },
							ShortVideoDescriptor { payload: 22 },
							ShortVideoDescriptor { payload: 1 },
							ShortVideoDescriptor { payload: 20 },
							ShortVideoDescriptor { payload: 31 },
							ShortVideoDescriptor { payload: 18 },
							ShortVideoDescriptor { payload: 19 },
						],
					}),
					DataBlock::AudioBlock(AudioBlock {
						header: DataBlockHeader { header: 35 },
						descriptors: vec![ShortAudioDescriptor {
							format_and_channels: 9,
							sampling_frequences: 7,
							bitrate_or_bitdepth: 7,
						}],
					}),
					DataBlock::VendorSpecific(VendorSpecific {
						header: DataBlockHeader { header: 101 },
						identifier: [3, 12, 0],
						payload: vec![16, 0],
					}),
					DataBlock::SpeakerAllocation(SpeakerAllocation {
						header: DataBlockHeader { header: 131 },
						speakers: 1,
						reserved: [0, 0],
					}),
				],
				descriptors: vec![
					DetailedTiming {
						pixel_clock: 148500,
						horizontal_active_pixels: 1920,
						horizontal_blanking_pixels: 280,
						vertical_active_lines: 1080,
						vertical_blanking_lines: 45,
						horizontal_front_porch: 88,
						horizontal_sync_width: 44,
						vertical_front_porch: 4,
						vertical_sync_width: 5,
						horizontal_size: 531,
						vertical_size: 299,
						horizontal_border_pixels: 0,
						vertical_border_pixels: 0,
						features: 30,
					},
					DetailedTiming {
						pixel_clock: 74250,
						horizontal_active_pixels: 1920,
						horizontal_blanking_pixels: 280,
						vertical_active_lines: 540,
						vertical_blanking_lines: 22,
						horizontal_front_porch: 88,
						horizontal_sync_width: 44,
						vertical_front_porch: 2,
						vertical_sync_width: 5,
						horizontal_size: 531,
						vertical_size: 299,
						horizontal_border_pixels: 0,
						vertical_border_pixels: 0,
						features: 158,
					},
					DetailedTiming {
						pixel_clock: 74250,
						horizontal_active_pixels: 1280,
						horizontal_blanking_pixels: 370,
						vertical_active_lines: 720,
						vertical_blanking_lines: 30,
						horizontal_front_porch: 110,
						horizontal_sync_width: 40,
						vertical_front_porch: 5,
						vertical_sync_width: 5,
						horizontal_size: 531,
						vertical_size: 299,
						horizontal_border_pixels: 0,
						vertical_border_pixels: 0,
						features: 30,
					},
					DetailedTiming {
						pixel_clock: 27000,
						horizontal_active_pixels: 720,
						horizontal_blanking_pixels: 138,
						vertical_active_lines: 480,
						vertical_blanking_lines: 45,
						horizontal_front_porch: 16,
						horizontal_sync_width: 62,
						vertical_front_porch: 9,
						vertical_sync_width: 6,
						horizontal_size: 531,
						vertical_size: 299,
						horizontal_border_pixels: 0,
						vertical_border_pixels: 0,
						features: 24,
					},
				],
			}),
		};

		test(d, &expected);
	}
}
