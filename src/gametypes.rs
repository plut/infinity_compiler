use crate::{Resref,Strref};
use macros::{Pack, Table, produce_resource_list};
#[derive(Debug,Default,Clone)] pub struct GameString<T: AsRef<str>> {
	// Note: since &str is not FromSql, we cannot use Table for this;
	// we must instead code the equivalent by hand
	pub flags: u16,
	pub sound: Resref,
	pub volume: i32,
	pub pitch: i32,
	pub string: T
}
#[derive(Debug,Pack,Table)]
#[resource(item_effects,itemref,items)] pub struct ItemEffect {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, Option<i64>, r#"references "item_abilities"("abref")"#)]
	pub opcode: u16, //opcode,
	pub target: u8, // EffectTarget,
	pub power: u8,
	pub parameter1: u32,
	pub parameter2: u32,
	pub timing_mode: u8, // TimingMode,
	pub dispel_mode: u8, // DispelMode,
	pub duration: u32,
	pub proba1: u8,
	pub proba2: u8,
	pub resource: Resref,
	pub dice_thrown: i32,
	pub dice_sides: i32,
	pub saving_throw_type: u32,
	pub saving_throw_bonus: i32,
	pub stacking_id: u32,
}
#[derive(Debug,Pack,Table)]
#[resource(item_abilities,itemref,items)] pub struct ItemAbility {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, auto, "primary key")]
	pub attack_type: u8, // AttackType,
	pub must_identify: u8,
	pub location: u8,
	pub alternative_dice_sides: u8,
	pub use_icon: Resref,
	pub target_type: u8, // TargetType,
	pub target_count: u8,
	pub range: u16,
	pub launcher_required: u8,
	pub alternative_dice_thrown: u8,
	pub speed_factor: u8,
	pub alternative_damage_bonus: u8,
	pub thac0_bonus: u16,
	pub dice_sides: u8,
	pub primary_type: u8,
	pub dice_thrown: u8,
	pub secondary_type: u8,
	pub damage_bonus: u16,
	pub damage_type: u16, // DamageType,
#[column(false)] pub effect_count: u16, // = 0,
#[column(false)] pub effect_index: u16, // = 0,
	max_charges: u16,
	depletion: u16,
	flags: u32,
	projectile_animation: u16,
	overhand_chance: u16,
	backhand_chance: u16,
	thrust_chance: u16,
	is_arrow: u16,
	is_bolt: u16,
	is_bullet: u16,
}
#[derive(Debug,Pack,Table)]
#[resource(items,itemref,items)] pub struct Item {
#[header("ITM V1  ")]
#[column(itemref, Resref, "primary key")]
	pub unidentified_name: Strref,
	pub name: Strref,
	pub replacement: Resref,
	pub flags: u32, // ItemFlags,
	pub itemtype: u16, // ItemType,
	pub usability: u32, // UsabilityFlags,
	pub animation: u16, // StaticString<2>,
	pub min_level: u16,
	pub min_strength: u16,
	pub min_strengthbonus: u8,
	pub kit1: u8,
	pub min_intelligence: u8,
	pub kit2: u8,
	pub min_dexterity: u8,
	pub kit3: u8,
	pub min_wisdom: u8,
	pub kit4: u8,
	pub min_constitution: u8,
	pub proficiency: u8, // WProf,
	pub min_charisma: u16,
	pub price: u32,
	pub stack_amount: u16,
	pub inventory_icon: Resref,
	pub lore: u16,
	pub ground_icon: Resref,
	pub weight: i32,
	pub unidentified_description: Strref,
	pub description: Strref,
	pub description_icon: Resref,
	pub enchantment: i32,
#[column(false)] pub abilities_offset: u32,
#[column(false)] pub abilities_count: u16,
#[column(false)] pub effect_offset: u32,
#[column(false)] pub effect_index: u16,
#[column(false)] pub equip_effect_count: u16,
}

// This last invocation closes the list of resources above, generating:
//  - the `ByResource<T>` type constructor,
//  - its implementation of `iter()` and an iterator
//  - and the constant holding the parent resources.
produce_resource_list!();
