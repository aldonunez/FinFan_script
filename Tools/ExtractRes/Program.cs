﻿/*
   Copyright 2012 Aldo J. Nunez

   Licensed under the Apache License, Version 2.0.
   See the LICENSE text file for details.
*/

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Drawing;
using System.Drawing.Imaging;
using System.Diagnostics;
using System.Globalization;
using System.Security.Cryptography;

namespace ExtractRes
{
    class Program
    {
        delegate void Extractor( Options options );

        static byte[] RomMd5U =
        {
            0x24, 0xAE, 0x5E, 0xDF, 0x83, 0x75, 0x16, 0x2F,
            0x91, 0xA6, 0x84, 0x6D, 0x32, 0x02, 0xE3, 0xD6
        };

        const int LevelXP = 0x2d010;
        const int ClassInitStats = 0x3050;
        const int ItemNamesBase = 0x2b710;
        const int ItemNamesPageBase = 0x20010;
        const int BattleCHRBase = 0x1c010;
        const int BattlePalettesBase = 0x30f30;
        const int EnemyNamesBase = 0x2d4f0;
        const int EnemyNamesPageBase = 0x24010;
        const int EnemyAttrs = 0x30530;
        const int AttackLists = 0x31030;
        const int FormationsBase = 0x2c410;
        // how the columns are arranged, 14 bytes
        const int BackdropLayout = 0x3f3c1;
        const int BackdropPal = 0x3210;
        const int BattleSpriteCHR = 0x25010;
        const int WeaponCHR = BattleSpriteCHR + 12 * 0x200;
        const int MagicCHR = BattleSpriteCHR + 12 * 0x200 + 0x300;
        const int EffectCHR = 0x26f10;
        const int FontCHR = 0x24810;
        const int BattleSpritePalettes = 0x3ebb5;
        const int BattleSpritePalIndexes = 0x3ecb4;
        const int MagicPermissions = 0x3ad28;
        const int WeaponPermissions = 0x3bf60;
        const int ArmorPermissions = 0x3bfb0;
        const int ArmorTypes = 0x3bce1;
        const int WeaponStats = 0x30010;
        const int ArmorStats = 0x30150;
        const int MagicStats = 0x301f0;
        const int SpecialStats = MagicStats + 8 * (64 + 2);
        const int SpecialNamesBase = 0x2b610;
        const int SpecialNamesPageBase = 0x20010;
        const int ObjectSpriteIndexes = 0x2e10;
        const int LPalettes = 0x2010;
        const int MapObjectCHR = 0xa210;
        const int MapObjects = 0x3410;
        const int MapManMainColors = 0x3b0;
        const int OverworldPalettes = 0x390;
        const int MapManCHR = 0x9010;
        const int OWObjectCHR = 0x9c10;
        const int OWTileCHR = 0x8010;
        const int OWTileBuild = 0x110;
        const int OWTileAttr = 0x310;
        const int LTileCHR = 0xc010;
        const int LTileBuild = 0x1010;
        const int LTileAttr = 0x410;
        const int TilesetIndexes = 0x2cd0;
        const int TilesetOrigMusic = 0x3cfd3;
        const int OWTilesetAttr = 0x10;
        const int OWMapRowTable = 0x4010;
        const int Domains = 0x2c010;
        const int FormationWeights = 0x3c59c;
        const int TileBackdrops = 0x3310;
        const int BattleRates = 0x2cc10;
        // entry teleport: overworld -> level
        // 32 entries indexed by teleport ID
        const int EnterTeleportsX = 0x2c10;
        const int EnterTeleportsY = 0x2c30;
        const int EnterTeleportsMap = 0x2c50;
        const int LTilesetAttr = 0x810;
        const int LMapTable = 0x10010;
        // exit teleport: level -> overworld
        // 16 entries indexed by teleport ID
        const int ExitTeleportsX = 0x2c70;
        const int ExitTeleportsY = 0x2c80;
        // swap teleport: level -> level
        // 64 entries indexed by teleport ID
        const int SwapTeleportsX = 0x2d10;
        const int SwapTeleportsY = 0x2d50;
        const int SwapTeleportsMap = 0x2d90;
        const int DialogueText = 0x28010;
        const int DialogueTextPage = 0x20010;
        const int ObjectTalkData = 0x395e5;
        const int InitFlags = 0x2f10;
        const int Treasures = 0x3110;
        const int Prices = 0x37c10;
        const int MenuText = 0x38510;
        const int MenuTextPage = 0x30010;
        const int OrbCHR = 0x37610;
        const int ShopTypes = 0x3ebc5;
        const int ShopData = 0x38310;
        const int ShopText = 0x38010;
        const int ShopTextPage = 0x30010;
        const int IntroText = 0x37f30;
        const int StoryText = 0x36810;
        const int StoryTextPage = 0x2C010;

        const int TileSize = 16;
        const int ChrSectionSize = 0x800;

        const int FlyerObjFlag = 0x20;

        static byte[] tileBuf = new byte[TileSize];

        static void Main( string[] args )
        {
            var options = Options.Parse( args );

            if ( options.Error != null )
            {
                Console.Error.WriteLine( options.Error );
                return;
            }

            if ( options.Function == null )
            {
                Console.WriteLine( "Nothing to work on." );
                return;
            }

            Directory.CreateDirectory( options.OutPath );

            CheckSupportedRom( options );

            Dictionary<string, Extractor> extractorMap = new Dictionary<string, Extractor>();

            extractorMap.Add( "enemies", ExtractEnemies );
            extractorMap.Add( "backdrops", ExtractBackdrops );
            extractorMap.Add( "battlesprites", ExtractBattleSprites );
            extractorMap.Add( "mapobjects", ExtractMapObjectsBundle );
            extractorMap.Add( "scripts", ExtractScripts );
            extractorMap.Add( "overworldtiles", ExtractOverworldTilesBundle );
            extractorMap.Add( "leveltiles", ExtractLevelTilesBundle );
            extractorMap.Add( "font", ExtractFont );
            extractorMap.Add( "player", PlayerExtractor.Extract );
            extractorMap.Add( "songs", ExtractSongs );
            extractorMap.Add( "sfx", ExtractSoundEffectsBundle );
            extractorMap.Add( "menus", ExtractMenusBundle );

            Extractor extractor = null;

            if ( options.Function == "all" )
            {
                foreach ( var pair in extractorMap )
                {
                    Console.WriteLine( "Extracting {0} ...", pair.Key );
                    pair.Value( options );
                }
            }
            else if ( extractorMap.TryGetValue( options.Function, out extractor ) )
            {
                Console.WriteLine( "Extracting {0} ...", options.Function );
                extractor( options );
            }
            else
            {
                Console.Error.WriteLine( "Function not supported: {0}", options.Function );
            }
        }

        private static void CheckSupportedRom( Options options )
        {
            if ( !File.Exists( options.RomPath ) )
            {
                Console.WriteLine( "ROM not found" );
                Environment.Exit( 1 );
            }

            byte[] romImage = File.ReadAllBytes( options.RomPath );
            byte[] hash;

            if ( romImage.Length < 0x40010 ||
                romImage[0] != 'N' || romImage[1] != 'E' || romImage[2] != 'S' || romImage[3] != 0x1A )
            {
                Console.WriteLine( "Input file is not a valid ROM." );
                Environment.Exit( 1 );
            }

            using ( var hashAlgo = MD5Cng.Create() )
            {
                hash = hashAlgo.ComputeHash( romImage, 0x10, romImage.Length - 0x10 );
            }

            if ( !AreHashesEqual( hash, RomMd5U ) )
            {
                Console.WriteLine( "ROM is not supported. Pass the (U) version." );
                Environment.Exit( 1 );
            }

            bool AreHashesEqual( byte[] a, byte[] b )
            {
                for ( int i = 0; i < a.Length; i++ )
                {
                    if ( a[i] != b[i] )
                        return false;
                }
                return true;
            }
        }

        static void ExtractMapObjectsBundle( Options options )
        {
            ExtractMapObjects( options );
            ExtractDialogue( options );
            ExtractTalkData( options );
            ExtractInitFlags( options );
            ExtractTreasures( options );
            ExtractPrices( options );
        }

        static void ExtractOverworldTilesBundle( Options options )
        {
            ExtractOverworldTiles( options );
            ExtractOverworldTileAttrs( options );
            ExtractOverworldMap( options );
            ExtractDomains( options );
            ExtractEnterTeleports( options );
        }

        static void ExtractLevelTilesBundle( Options options )
        {
            ExtractLevelTiles( options );
            ExtractLevelTileAttrs( options );
            ExtractLevelMaps( options );
            ExtractExitTeleports( options );
            ExtractSwapTeleports( options );
            ExtractLevelBattleRates( options );
            ExtractLevelMusic( options );
        }

        static void ExtractSoundEffectsBundle( Options options )
        {
            ExtractSoundEffects( options );
        }

        static void ExtractMenusBundle( Options options )
        {
            ExtractMenu( options );
            ExtractShops( options );
            ExtractStory( options );
            ExtractStoryBackgrounds( options );
            ExtractTheEnd( options );
        }

        struct Enemy
        {
            public string Name;
            public int Pattern;
            public int Pic;
            public int Pal;
        }

        class Palette
        {
            public byte[] Indexes;

            public Palette()
            {
                Indexes = new byte[4];
            }

            public Palette( byte c0, byte c1, byte c2, byte c3 )
            {
                Indexes = new byte[4] { c0, c1, c2, c3 };
            }

            public override bool Equals( object obj )
            {
                if ( !(obj is Palette) )
                    return false;
                Palette other = (Palette) obj;
                return other.Indexes[0] == Indexes[0]
                    && other.Indexes[1] == Indexes[1]
                    && other.Indexes[2] == Indexes[2]
                    && other.Indexes[3] == Indexes[3];
            }

            public override int GetHashCode()
            {
                return Indexes[0] + Indexes[1] + Indexes[2] + Indexes[3];
            }
        }

        class Tileset
        {
            public int Pattern;
            public Palette[] Palettes;
            public int PaletteIndex;

            public override bool Equals( object obj )
            {
                if ( !(obj is Tileset) )
                    return false;
                Tileset other = (Tileset) obj;
                if ( Pattern != other.Pattern )
                    return false;
                for ( int i = 0; i < 4; i++ )
                    if ( !other.Palettes[i].Equals( Palettes[i] ) )
                        return false;
                return true;
            }

            public override int GetHashCode()
            {
                int sum = 0;
                for ( int i = 0; i < 4; i++ )
                    sum += Palettes[i].GetHashCode();
                return sum | (Pattern << 16);
            }
        }

        static byte[] fontMapping = new byte[128];
        static byte[] itemTarget = new byte[256];
        static byte[] magicTarget = new byte[64];

        static Program()
        {
            for ( int i = 0; i < 10; i++ )
                fontMapping[i + '0'] = (byte) (0 + i);
            for ( int i = 0; i < 26; i++ )
                fontMapping[i + 'A'] = (byte) (10 + i);
            for ( int i = 0; i < 26; i++ )
                fontMapping[i + 'a'] = (byte) (36 + i);

            fontMapping['\''] = 62;
            fontMapping[','] = 63;
            fontMapping['.'] = 64;
            fontMapping[' '] = 65;
            fontMapping['-'] = 66;
            fontMapping['_'] = 67;
            fontMapping['!'] = 68;
            fontMapping['?'] = 69;
            fontMapping['%'] = 96;
            fontMapping['/'] = 127;
            fontMapping[':'] = 143;

            for ( int i = 0; i < 13; i++ )
                fontMapping[0x10 + i] = (byte) (84 + i);

            fontMapping[0x1C] = 97;     // potion

            for ( int i = 0; i < 8; i++ )
                fontMapping[i + 4] = (byte) (0x77 + i);

            itemTarget[25] = 1;
            itemTarget[26] = 1;
            itemTarget[27] = 1;

            magicTarget[0] = 1;
            magicTarget[16] = 1;
            //magicTarget[19] = 1;
            magicTarget[24] = 1;
            magicTarget[32] = 1;
            magicTarget[33] = 1;
            //magicTarget[35] = 1;
            //magicTarget[38] = 1;
            magicTarget[40] = 1;
            //magicTarget[41] = 1;
            magicTarget[48] = 1;
            //magicTarget[51] = 1;
            magicTarget[56] = 1;
        }

        private static void ExtractStoryBackground( 
            BinaryReader reader,
            Bitmap bmp,
            int tilesBase, 
            int nameTableBase, 
            Color[][] fullPal,
            bool blankBox,
            int blankTile )
        {
            reader.BaseStream.Position = nameTableBase;

            byte[] nameTable = reader.ReadBytes( 32 * 30 );
            byte[] attrTable = reader.ReadBytes( 64 );

            for ( int i = 0; i < 30; i++ )
            {
                for ( int j = 0; j < 32; j++ )
                {
                    int tiles = tilesBase;
                    int t = nameTable[i * 32 + j];
                    int attrByteIndex = i / 4 * 8 + j / 4;
                    int attrBitIndex = (i % 4) / 2 * 2 + (j % 4) / 2;
                    int palIndex = (attrTable[attrByteIndex] >> (attrBitIndex * 2)) & 3;

                    if ( t >= 128 )
                    {
                        if ( blankBox )
                        {
                            t = blankTile;
                            palIndex = 1;
                        }
                        else
                        {
                            t -= 128;
                            tiles = FontCHR;
                        }
                    }

                    Color[] colors = fullPal[palIndex];

                    reader.BaseStream.Position = tiles + t * 16;
                    DrawTile( reader, bmp, colors, j * 8, i * 8 );
                }
            }
        }

        private static void ExtractStoryBackgrounds( Options options )
        {
            const int EndingTiles = 0x2E810;
            const int EndingNameTable = 0x2E810 + 128 * 16;
            const int EndingAttrTable = 0x2E810 + 128 * 16 + (32 * 30);
            const int OpeningTiles = 0x2F410;
            const int OpeningNameTable = 0x2F410 + 128 * 16;
            const int OpeningAttrTable = 0x2F410 + 128 * 16 + (32 * 30);

            Bitmap bmp = new Bitmap( 256, 240 );

            Color[][] endingPal = new Color[][]
            {
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x00],
                    DefaultSystemPalette.Colors[0x01],
                    DefaultSystemPalette.Colors[0x30]
                },
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x32],
                    DefaultSystemPalette.Colors[0x21],
                    DefaultSystemPalette.Colors[0x30]
                },
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x2C],
                    DefaultSystemPalette.Colors[0x2A],
                    DefaultSystemPalette.Colors[0x1A]
                },
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x00],
                    DefaultSystemPalette.Colors[0x0f],
                    DefaultSystemPalette.Colors[0x30]
                }
            };

            Color[][] openingPal = new Color[][]
            {
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x00],
                    DefaultSystemPalette.Colors[0x02],
                    DefaultSystemPalette.Colors[0x30]
                },
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x3b],
                    DefaultSystemPalette.Colors[0x11],
                    DefaultSystemPalette.Colors[0x24]
                },
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x3b],
                    DefaultSystemPalette.Colors[0x0b],
                    DefaultSystemPalette.Colors[0x2b]
                },
                new Color[4]
                {
                    DefaultSystemPalette.Colors[0x0F],
                    DefaultSystemPalette.Colors[0x00],
                    DefaultSystemPalette.Colors[0x0f],
                    DefaultSystemPalette.Colors[0x30]
                }
            };

            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                bool blank = false;

                ExtractStoryBackground( 
                    reader, 
                    bmp, 
                    EndingTiles, 
                    EndingNameTable, 
                    endingPal, 
                    blank, 
                    1 );
                bmp.Save( options.MakeOutPath( @"ending.png" ), ImageFormat.Png );

                ExtractStoryBackground( 
                    reader, 
                    bmp, 
                    OpeningTiles, 
                    OpeningNameTable, 
                    openingPal, 
                    blank, 
                    109 );
                bmp.Save( options.MakeOutPath( @"opening.png" ), ImageFormat.Png );
            }
        }

        private static void ExtractTheEnd( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = 0x36010;
                byte[] prog = reader.ReadBytes( 0x400 );
                File.WriteAllBytes( options.MakeOutPath( "theEndProg.dat" ), prog );

                reader.BaseStream.Position = 0x365A4;
                byte[] mask = reader.ReadBytes( 0x50 );
                File.WriteAllBytes( options.MakeOutPath( "theEndMask.dat" ), mask );
            }
        }

        static readonly string[] ShopMessages = 
        {
            "WEAPON",
            "ARMOR",
            "WMAGIC",
            "BMAGIC",
            "CLINIC",
            " INN",
            " ITEM",
            "OASIS",
            "",
            "Welcome",
            "What do you want?",
            "What would you \nlike to sell?",
            "You can't afford \nthat.",
            "You can't carry \nany more.",
            "How many would you \nlike?",
            "Too bad __ \nSomething else?",
            "Thank you!",
            "How many are you \nselling?",
            "Which spell?",
            "Who will learn the \nspell?",
            "Will that be all?",
            "You can't learn \nthat.",
            "You already know \nthat spell.",
            "This spell level \nis full.",
            "Thank you! What \nelse?",
            "Who shall be \nrevived for %u G?",
            "You do not need \nmy help now.",
            "WARRIOR__ Return to \nlife!",
            "Stay the night for \n%u G?",
            "Pleasant dreams!",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
        };

        private static void ExtractShops( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = ShopTypes;
                byte[] shopTypes = reader.ReadBytes( 71 );
                File.WriteAllBytes( options.MakeOutPath( @"shopTypes.dat" ), shopTypes );

                ushort[] shopPtrs = new ushort[71];

                reader.BaseStream.Position = ShopData;

                for ( int i = 0; i < shopPtrs.Length; i++ )
                {
                    shopPtrs[i] = reader.ReadUInt16();
                }

                string ShopFile = options.MakeOutPath( @"shopStock.tab" );
                byte[] data = reader.ReadBytes( 0x180 - (71 * 2) );

                using ( BinaryWriter writer = new BinaryWriter( File.Create( ShopFile ) ) )
                {
                    for ( int i = 0; i < shopPtrs.Length; i++ )
                    {
                        ushort ptr = (ushort) (shopPtrs[i] - shopPtrs[0]);
                        writer.Write( ptr );
                    }

                    writer.Write( data );
                }

                string[] shopText = Text.ReadStringTable( reader, ShopText, ShopTextPage, 38 );
                WriteStringTableFile( options.MakeOutPath( @"shopText.tab" ), ShopMessages );
            }
        }

        static readonly string[] Credits = new string[]
        {
            "\n   PROGRAMMED\n\n       BY\n\n\n   N A S I R",
            "\n   CHARACTER\n\n    DESIGN\n\n\n YOSITAKA AMANO",
            "\n    SCENARIO\n\n       BY\n\n\n  KENJI TERADA",
            "\n   PRODUCTION\n\n       OF\n\n\n SQUARE  A-TEAM"
        };

        private static void ExtractStory( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                string[] storyText = Text.ReadStringTable( reader, StoryText, StoryTextPage, 25 );

#if A
                Array.Resize( ref storyText, storyText.Length + 1 );
                storyText[storyText.Length - 1] = Text.DecodeString( reader, IntroText );

                WriteStringTableFile( options.MakeOutPath( @"storyText.tab" ), storyText );
#else
                string[] story = new string[storyText.Length + 1 + Credits.Length];
                Array.Copy( storyText, story, 4 );
                Array.Copy( Credits, 0, story, 4, Credits.Length );
                Array.Copy( storyText, 4, story, 4 + Credits.Length, storyText.Length - 4 );
                story[story.Length - 1] = Text.DecodeString( reader, IntroText );

                WriteStringTableFile( options.MakeOutPath( @"storyText.tab" ), story );
#endif
            }
        }

        private static void ExtractMenu( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                string[] menuText = Text.ReadStringTable( reader, MenuText, MenuTextPage, 64 );
                WriteStringTableFile( options.MakeOutPath( @"menuText.tab" ), menuText );

                Bitmap bmp = new Bitmap( 128, 16 );
                Palette darkPal = new Palette( 0, 0, 1, 0x30 );
                Palette lightPal = new Palette( 0, 0x30, 1, 0x22 );

                for ( int i = 0; i < 5; i++ )
                {
                    int ptr = OrbCHR + TileSize * 4 * i;
                    DrawSprite( reader, ptr, bmp, lightPal, 2, 2, i * 16, 0, false );
                }

                DrawSprite( reader, OrbCHR + TileSize * 22, bmp, darkPal, 2, 2, 80, 0, false );

                DrawSprite( reader, OrbCHR + TileSize * 20, bmp, lightPal, 2, 1, 96, 0, false );
                DrawSprite( reader, OrbCHR + TileSize * 20, bmp, darkPal, 2, 1, 96, 8, false );

                bmp.Save( options.MakeOutPath( @"menu.png" ) );
            }
        }

        private static void ExtractPrices( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = Prices;
                byte[] priceBuf = reader.ReadBytes( 2 * 256 );

                File.WriteAllBytes( options.MakeOutPath( @"prices.dat" ), priceBuf );
            }
        }

        private static void ExtractTreasures( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = Treasures;
                byte[] treasuresBuf = reader.ReadBytes( 256 );

                File.WriteAllBytes( options.MakeOutPath( @"treasure.dat" ), treasuresBuf );
            }
        }

        private static void ExtractInitFlags( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = InitFlags;
                byte[] flagsBuf = reader.ReadBytes( 256 );

                File.WriteAllBytes( options.MakeOutPath( @"initFlags.dat" ), flagsBuf );
            }
        }

        static string[] scriptNames = new string[]
        {
            "None",
            "KingConeria",
            "Garland",
            "Princess1",
            "Bikke",
            "ElfDoc",
            "ElfPrince",
            "Astos",
            "Nerrick",
            "Smith",
            "Matoya",
            "Unne",
            "Vampire",
            "Sarda",
            "Bahamut",
            "TalkIfVisible",
            "SubEng",
            "CubeBot",
            "Princess2",
            "Fairy",
            "Titan",
            "CanoeSage",
            "Talk",
            "Talk",
            "Replace",
            "Replace",
            "TalkFight",
            "TalkFight",
            "TalkFight",
            "TalkFight",
            "TalkFight",
            "None",
            "TalkIfVisible",
            "TalkIfVisible",
            "TalkIfVisible",
            "TalkIfItem",
            "TalkIfVisible",
            "TalkIfVisible",
            "Invis",
            "IfBridge",
            "TalkIfVisible",
            "TalkIfVisible",
            "TalkIfVisible",
            "TalkIfVisible",
            "TalkIfItem",
            "TalkIfVisible",
            "TalkIfItem",
            "TalkIfEvent",
            "TalkIfVisible",
            "TalkIfVisible",
            "GoBridge",
            "TalkIfVisible",
            "_4Orb",
            "Talk",
            "Talk",
            "TalkIfVisible",
            "TalkIfVisible",
            "Talk",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "Talk",
            "Talk",
            "Talk",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "Talk",
            "TalkIfEvent",
            "TalkIfItem",
            "Talk",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfItem",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "Talk",
            "IfCanoe",
            "TalkIfItem",
            "TalkIfEvent",
            "TalkIfEvent",
            "Talk",
            "Talk",
            "IfCanal",
            "Talk",
            "Talk",
            "TalkIfItem",
            "TalkIfItem",
            "Talk",
            "IfCanal",
            "IfKeyTnt",
            "Talk",
            "IfCanal",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "TalkIfVisible",
            "Talk",
            "IfEarthVamp",
            "TalkIfItem",
            "TalkIfVisible",
            "IfEarthVamp",
            "Talk",
            "Talk",
            "TalkIfItem",
            "IfAirship",
            "Talk",
            "TalkIfEvent",
            "TalkIfItem",
            "Talk",
            "Talk",
            "Talk",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "_4Orb",
            "TalkIfItem",
            "IfEarthFire",
            "TalkIfItem",
            "Talk",
            "Talk",
            "CoOGuy",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "TalkIfItem",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "TalkIfItem",
            "TalkIfEvent",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "CubeBotBad",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "TalkIfItem",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "TalkIfEvent",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Chime",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "TalkIfEvent",
            "BlackOrb",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "Talk",
            "SampleMovement",
        };

        private static void ExtractScripts( Options options )
        {
            string compilerPath = options.MakeOutPath( "GeminiC.exe" );
            string thisPath = Path.GetDirectoryName( Process.GetCurrentProcess().MainModule.FileName );
            string scriptFile = Path.Combine( thisPath, @"Data\ObjEvents.gema" );
            string outBin = options.MakeOutPath( "objEvents.prg" );
            string outIndex = options.MakeOutPath( "objEvents.prgidx" );
            string args = string.Format( "\"{0}\" \"{1}\" \"{2}\"", scriptFile, outBin, outIndex );
            using ( var process = Process.Start( compilerPath, args ) )
            {
                process.WaitForExit();
                if ( process.ExitCode != 0 )
                    throw new Exception( "The script compiler failed with code " + process.ExitCode );

                var nameToAddr = new Dictionary<string, int>();

                using ( var reader = new BinaryReader( File.OpenRead( outIndex ) ) )
                {
                    while ( reader.BaseStream.Position < reader.BaseStream.Length )
                    {
                        int address = reader.ReadInt32();
                        var chars = reader.ReadChars( 32 );
                        string name = new string( chars ).TrimEnd( '\0' );

                        nameToAddr.Add( name, address );
                    }
                }

                string indexFile = options.MakeOutPath( "objEvents.dat" );
                using ( var writer = new BinaryWriter( TruncateFile( indexFile ) ) )
                {
                    foreach ( var name in scriptNames )
                    {
                        writer.Write( (ushort) nameToAddr[name] );
                    }
                }
            }
        }

        private static void ExtractTalkData( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                byte[] data = new byte[(208 + 8) * 4];

                reader.BaseStream.Position = ObjectTalkData;
                reader.Read( data, 0, 208 * 4 );

                for ( int i = 0; i < 8; i++ )
                {
                    int baseIndex = (208 + i) * 4;

                    for ( int j = 0; j < 4; j++ )
                    {
                        data[baseIndex + j] = data[0x57 * 4 + j];
                    }
                }

                File.WriteAllBytes( options.MakeOutPath( @"talkParams.dat" ), data );
            }
        }

        private static void ExtractDialogue( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                string[] msgs = Text.ReadStringTable( reader, DialogueText, DialogueTextPage, 256 );

                string DialogueFile = options.MakeOutPath( @"dialogue.tab" );

                WriteStringTableFile( DialogueFile, msgs );
            }
        }

        private static void ExtractLevelBattleRates( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = BattleRates;
                byte[] rates = reader.ReadBytes( 64 );

                string RatesFile = options.MakeOutPath( @"battleRates.dat" );

                File.WriteAllBytes( RatesFile, rates );
            }
        }

        private static void ExtractSwapTeleports( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = SwapTeleportsX;
                byte[] xs = reader.ReadBytes( 64 );
                byte[] ys = reader.ReadBytes( 64 );
                byte[] mapIds = reader.ReadBytes( 64 );

                string TeleportsFile = options.MakeOutPath( @"swapTeleports.dat" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( TeleportsFile ) ) )
                {
                    writer.BaseStream.SetLength( 0 );

                    for ( int i = 0; i < 64; i++ )
                    {
                        writer.Write( xs[i] );
                        writer.Write( ys[i] );
                        writer.Write( mapIds[i] );
                        writer.Write( (byte) 0 );
                    }
                }
            }
        }

        private static void ExtractExitTeleports( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = ExitTeleportsX;
                byte[] xs = reader.ReadBytes( 16 );
                byte[] ys = reader.ReadBytes( 16 );

                string TeleportsFile = options.MakeOutPath( @"exitTeleports.dat" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( TeleportsFile ) ) )
                {
                    writer.BaseStream.SetLength( 0 );

                    for ( int i = 0; i < 16; i++ )
                    {
                        writer.Write( xs[i] );
                        writer.Write( ys[i] );
                    }
                }
            }
        }

        private static void ExtractLevelMaps( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                ushort[] mapPtrs = new ushort[64];

                reader.BaseStream.Position = LMapTable;

                for ( int i = 0; i < mapPtrs.Length; i++ )
                {
                    mapPtrs[i] = reader.ReadUInt16();
                }

                // there are really 61 maps, but there's space in the table for 64

                int firstMapPos = (int) reader.BaseStream.Position;
                int lastMapOffset = mapPtrs[60] - mapPtrs[0];

                // look for the end of the last map

                reader.BaseStream.Position += lastMapOffset;

                while ( reader.ReadByte() != 0xff )
                {
                }

                // now that we know where all map data ends, read it all

                int mapDataSize = (int) reader.BaseStream.Position - firstMapPos;

                reader.BaseStream.Position = firstMapPos;
                byte[] mapData = reader.ReadBytes( mapDataSize );

                string LMapsFile = options.MakeOutPath( @"levelMaps.tab" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( LMapsFile ) ) )
                {
                    for ( int i = 0; i < mapPtrs.Length; i++ )
                    {
                        // first byte of map data is offset 0
                        writer.Write( (ushort) (mapPtrs[i] - mapPtrs[0]) );
                    }

                    writer.Write( mapData );
                }
            }
        }

        private static void ExtractLevelTileAttrs( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = LTilesetAttr;
                byte[] attrBuf = reader.ReadBytes( 8 * 128 * 2 );
                File.WriteAllBytes( options.MakeOutPath( @"levelTileAttr.dat" ), attrBuf );

                reader.BaseStream.Position = TilesetIndexes;
                byte[] tileSetIDs = reader.ReadBytes( 64 );
                File.WriteAllBytes( options.MakeOutPath( @"levelTilesets.dat" ), tileSetIDs );
            }
        }

        private static void ExtractEnterTeleports( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = EnterTeleportsX;
                byte[] xs = reader.ReadBytes( 32 );
                byte[] ys = reader.ReadBytes( 32 );
                byte[] mapIds = reader.ReadBytes( 32 );

                string TeleportsFile = options.MakeOutPath( @"enterTeleports.dat" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( TeleportsFile ) ) )
                {
                    writer.BaseStream.SetLength( 0 );

                    for ( int i = 0; i < 32; i++ )
                    {
                        writer.Write( xs[i] );
                        writer.Write( ys[i] );
                        writer.Write( mapIds[i] );
                        writer.Write( (byte) 0 );
                    }
                }
            }
        }

        private static void ExtractDomains( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = Domains;
                byte[] domainsBuf = reader.ReadBytes( 128 * 8 );

                File.WriteAllBytes( options.MakeOutPath( @"domains.dat" ), domainsBuf );

                reader.BaseStream.Position = FormationWeights;
                byte[] weightsBuf = reader.ReadBytes( 64 );

                File.WriteAllBytes( options.MakeOutPath( @"formationWeights.dat" ), weightsBuf );

                reader.BaseStream.Position = TileBackdrops;
                byte[] backdropsBuf = reader.ReadBytes( 128 );

                File.WriteAllBytes( options.MakeOutPath( @"tileBackdrops.dat" ), backdropsBuf );
            }
        }

        struct LoopPoints
        {
            public int Begin;
            public int End;
        }

        static LoopPoints FindLoopPoints(
            BinaryReader reader,
            int dataBase,
            byte[] noteLenghts,
            ushort[] square1Ptr,
            ushort[] square2Ptr,
            ushort[] trianglePtr,
            int songId )
        {
            ushort firstPtr = square1Ptr[0];
            LoopPoints loop = new LoopPoints();
            LoopPoints[] l = new LoopPoints[3];

            l[0] = FindChannelLoopPoints( reader, dataBase, noteLenghts, square1Ptr, firstPtr, songId );
            l[1] = FindChannelLoopPoints( reader, dataBase, noteLenghts, square2Ptr, firstPtr, songId );
            l[2] = FindChannelLoopPoints( reader, dataBase, noteLenghts, trianglePtr, firstPtr, songId );

            for ( int i = 0; i < 3; i++ )
            {
                if ( l[i].End > loop.End )
                {
                    loop.Begin = l[i].Begin;
                    loop.End = l[i].End;
                }
            }

            return loop;
        }

        static LoopPoints FindChannelLoopPoints(
            BinaryReader reader,
            int dataBase,
            byte[] noteLenghts,
            ushort[] channelPtr,
            ushort firstPtr,
            int songId )
        {
            reader.BaseStream.Position = dataBase + (channelPtr[songId] - firstPtr);

            uint[] instFrames = new uint[0x1da0];
            int loopBeginFrames = -1;

            int runningFrames = 0;
            int tempoIndex = 0;
            int repeatCount = 0;

            while ( true )
            {
                int ptr = (int) reader.BaseStream.Position - dataBase;
                if ( instFrames[ptr] == 0 )
                    instFrames[ptr] = (uint) runningFrames | 0x80000000;

                byte b = reader.ReadByte();

                //Console.WriteLine( "{0:X2}", b );

                if ( b < 0xD0 )
                {
                    // notes and rests
                    int lenIndex = b & 0xF;
                    int lenFrames = noteLenghts[tempoIndex * 16 + lenIndex];

                    runningFrames += lenFrames;

                    //Console.WriteLine( "Note/rest, length {0}", lenFrames );
                }
                else if ( b == 0xD0 )
                {
                    int targetPtr = reader.ReadUInt16();
                    int targetPos = (targetPtr - firstPtr);

                    //Console.WriteLine( "Loop", b );
                    //Console.WriteLine( "Start loop running frames: {0}", 
                    //    instFrames[targetPos] & 0x7fffffff );

                    loopBeginFrames = (int) (instFrames[targetPos] & 0x7fffffff);
                    break;
                }
                else if ( b >= 0xD1 && b <= 0xD7 )
                {
                    int targetPtr = reader.ReadUInt16();
                    int repeats = b & 0xF;
                    int targetPos = dataBase + (targetPtr - firstPtr);

                    if ( repeatCount == 0 )
                    {
                        repeatCount = repeats;
                        reader.BaseStream.Position = targetPos;
                    }
                    else
                    {
                        repeatCount--;
                        if ( repeatCount > 0 )
                            reader.BaseStream.Position = targetPos;
                    }

                    //Console.WriteLine( "Loop: {0:X}", b );
                }
                else if ( b >= 0xF9 && b <= 0xFE )
                {
                    tempoIndex = b - 0xF9;
                    //Console.WriteLine( "Change tempo: {0:X}", b );
                }
                else if ( b == 0xF8 )
                {
                    b = reader.ReadByte();
                    //Console.WriteLine( "Change envelope speed: {0:X}", b );
                }
                else if ( b == 0xFF )
                {
                    //Console.WriteLine( "End" );
                    break;
                }
            }

            //Console.WriteLine( "End frames: {0}", runningFrames );

            return new LoopPoints() { Begin = loopBeginFrames, End = runningFrames };
        }

        private static LoopPoints[] ExtractLoopPoints( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                ushort[] square1Ptr = new ushort[24];
                ushort[] square2Ptr = new ushort[24];
                ushort[] trianglePtr = new ushort[24];

                reader.BaseStream.Position = 0x34010;

                for ( int i = 0; i < 24; i++ )
                {
                    square1Ptr[i] = reader.ReadUInt16();
                    square2Ptr[i] = reader.ReadUInt16();
                    trianglePtr[i] = reader.ReadUInt16();
                    reader.ReadUInt16();
                }

                int dataBase = (int) reader.BaseStream.Position;
                //byte[] data = new byte[0x1da0];

                reader.BaseStream.Position = 0x37369;
                byte[] noteLenghts = reader.ReadBytes( 16 * 6 );

                LoopPoints[] pointPairs = new LoopPoints[24];
                string LoopFile = options.MakeOutPath( @"loopPoints.dat" );
                using ( BinaryWriter writer = new BinaryWriter( File.Create( LoopFile ) ) )
                {
                    for ( int i = 0; i < 24; i++ )
                    {
                        LoopPoints loopPoints = FindLoopPoints(
                            reader,
                            dataBase,
                            noteLenghts,
                            square1Ptr,
                            square2Ptr,
                            trianglePtr,
                            i );
                        pointPairs[i] = loopPoints;

                        writer.Write( (short) loopPoints.Begin );
                        writer.Write( (short) loopPoints.End );
                    }
                }
                return pointPairs;
            }
        }

        class SoundItem
        {
            public short Track;
            public short Begin;
            public short End;
            public string Filename;
        }

        private static void ExtractSongs( Options options )
        {
            byte[] nsfImage = BuildMemoryNsf( options, "NsfSong.csv" );

            string[] songFilenames = 
            {
                "01_prelude.wav",
                "02_opening.wav",
                "03_ending.wav",
                "04_field.wav",
                "05_ship.wav",
                "06_airship.wav",
                "07_town.wav",
                "08_castle.wav",
                "09_volcano.wav",
                "10_matoya.wav",
                "11_dungeon.wav",
                "12_temple.wav",
                "13_sky.wav",
                "14_sea_shrine.wav",
                "15_shop.wav",
                "16_battle.wav",
                "17_menu.wav",
                "18_dead.wav",
                "19_victory.wav",
                "20_fanfare.wav",
                "21_unknown.wav",
                "22_save.wav",
                "23_unknown.wav"
            };

            LoopPoints[] loopPoints = ExtractLoopPoints( options );
            loopPoints[20].End = 30;
            for ( int i = 0; i < loopPoints.Length; i++ )
            {
                if ( i == songFilenames.Length )
                    break;

                SoundItem item = new SoundItem();
                item.Track = (short) i;
                item.Filename = songFilenames[i];
                item.Begin = (short) loopPoints[i].Begin;
                item.End = (short) loopPoints[i].End;
                ExtractSoundFile( nsfImage, options, item );
            }

            File.Copy( 
                options.MakeOutPath( "23_unknown.wav" ), 
                options.MakeOutPath( "ff1-sfx-potion.wav" ), 
                true );
        }

        struct SfxFileDesc
        {
            public string Filename;
            public int Track;
            public ushort End;
        }

        private static void ExtractSoundEffects( Options options )
        {
            byte[] nsfImage = BuildMemoryNsf( options, "nsf-sfx.csv" );

            SfxFileDesc[] effects = 
            {
                new SfxFileDesc { Filename = "ff1-sfx-confirm.wav", Track = 0, End = 31 },
                new SfxFileDesc { Filename = "ff1-sfx-cursor.wav", Track = 1, End = 16 },
                new SfxFileDesc { Filename = "ff1-sfx-door.wav", Track = 7, End = 0x25 },
                new SfxFileDesc { Filename = "ff1-sfx-error.wav", Track = 14, End = 16 },
                new SfxFileDesc { Filename = "ff1-sfx-fight.wav", Track = 12, End = 0x41 },
                new SfxFileDesc { Filename = "ff1-sfx-hurt.wav", Track = 18, End = 0xF },
                new SfxFileDesc { Filename = "ff1-sfx-magic.wav", Track = 17, End = 0x3C },
                //new SfxFileDesc { Filename = "ff1-sfx-potion.wav", Track = 3 },
                new SfxFileDesc { Filename = "ff1-sfx-step.wav", Track = 6, End = 6 },
                new SfxFileDesc { Filename = "ff1-sfx-strike.wav", Track = 19, End = 0xC },
                new SfxFileDesc { Filename = "ff1-sfx-ship.wav", Track = 4, End = 60 * 4 },
                new SfxFileDesc { Filename = "ff1-sfx-lift.wav", Track = 15, End = 0x21 * 2 },
                new SfxFileDesc { Filename = "ff1-sfx-land.wav", Track = 16, End = 0x21 * 2 },
                new SfxFileDesc { Filename = "ff1-sfx-airship.wav", Track = 3, End = 60 * 3 },
                new SfxFileDesc { Filename = "ff1-sfx-lava.wav", Track = 5, End = 5 },
                new SfxFileDesc { Filename = "ff1-sfx-chaos_rumble.wav", Track = 20, End = 2168 },
            };

            for ( int i = 0; i < effects.Length; i++ )
            {
                SoundItem item = new SoundItem();
                item.Track = (short) effects[i].Track;
                item.Filename = effects[i].Filename;
                item.Begin = 0;
                item.End = (short) effects[i].End;
                ExtractSoundFile( nsfImage, options, item );
            }

            File.Copy(
                options.MakeOutPath( "ff1-sfx-chaos_rumble.wav" ),
                options.MakeOutPath( "24_chaos_rumble.wav" ),
                true );
        }

        private static void ExtractSoundFile( string nsfPath, Options options, SoundItem item )
        {
            if ( string.IsNullOrEmpty( nsfPath ) )
                throw new Exception( "An NSF file is needed to extract sound files." );

            byte[] nsfImage = File.ReadAllBytes( nsfPath );

            ExtractSoundFile( nsfImage, options, item );
        }

        private static void ExtractSoundFile( byte[] nsfImage, Options options, SoundItem item )
        {
            const int SampleRate = 44100;
            const double SampleRateMs = SampleRate / 1000.0;
            const double MillisecondsAFrame = 1000.0 / 60.0;

            string outPath = options.MakeOutPath( item.Filename );
            using ( ExtractNsf.NsfEmu emu = new ExtractNsf.NsfEmu() )
            using ( ExtractNsf.WaveWriter waveWriter = new ExtractNsf.WaveWriter( SampleRate, outPath ) )
            {
                emu.SampleRate = SampleRate;
                emu.LoadMem( nsfImage, nsfImage.Length );
                emu.StartTrack( item.Track );

                waveWriter.EnableStereo();

                short[] buffer = new short[1024];
                int limit = (int) (item.End * MillisecondsAFrame);
                while ( emu.Tell < limit )
                {
                    int count = buffer.Length;
                    int samplesRem = (int) (SampleRateMs * (limit - emu.Tell));
                    if ( samplesRem < count )
                    {
                        count = (int) ((samplesRem + 1) & 0xFFFFFFFE);
                    }
                    emu.Play( count, buffer );
                    waveWriter.Write( buffer, count, 1 );
                }
            }
        }

        private class NsfChunkItem
        {
            public string SrcName;
            public int SrcAddr;
            public int DstAddr;
            public int Length;

            public static NsfChunkItem ConvertFields( string[] fields )
            {
                NsfChunkItem item = new NsfChunkItem();
                item.SrcName = fields[0];
                item.SrcAddr = int.Parse( fields[1], NumberStyles.HexNumber );
                item.DstAddr = int.Parse( fields[2], NumberStyles.HexNumber );
                item.Length = int.Parse( fields[3], NumberStyles.HexNumber );
                return item;
            }
        }

        private static byte[] BuildMemoryNsf( Options options, string tableFileName )
        {
            List<NsfChunkItem> nsfItems;

            using ( var specStream = GetResourceStream( "ExtractRes.Data." + tableFileName ) )
            {
                nsfItems = DatafileReader.ReadTable( specStream, NsfChunkItem.ConvertFields );
            }

            if ( nsfItems.Count == 0 || nsfItems[0].SrcName != "" )
                throw new Exception();

            byte[] nsfImage = null;
            var romImage = File.ReadAllBytes( options.RomPath );

            foreach ( var item in nsfItems )
            {
                if ( item.SrcName == "" )
                {
                    nsfImage = new byte[item.Length];
                }
                else if ( string.Compare( item.SrcName, "rom", true ) == 0 )
                {
                    Array.Copy( romImage, item.SrcAddr, nsfImage, item.DstAddr, item.Length );
                }
                else
                {
                    using ( var stream = GetResourceStream( "ExtractRes.Data." + item.SrcName ) )
                    {
                        stream.Position = item.SrcAddr;
                        stream.Read( nsfImage, item.DstAddr, item.Length );
                    }
                }
            }

            return nsfImage;
        }

        private static void ExtractFont( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                string FontFile = options.MakeOutPath( @"main.mfont" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( FontFile ) ) )
                {
                    foreach ( byte index in fontMapping )
                    {
                        byte x = (byte) ((index % 16) * 8);
                        byte y = (byte) ((index / 16) * 8);
                        writer.Write( x );
                        writer.Write( y );
                    }
                }

                reader.BaseStream.Position = FontCHR;

                // add two rows to hold the cursor
                Bitmap bmp = new Bitmap( 16 * 8, (8 + 2) * 8 );
                Color[] colors = new Color[]
                {
                    DefaultSystemPalette.Colors[0],
                    DefaultSystemPalette.Colors[0],
                    DefaultSystemPalette.Colors[1],
                    DefaultSystemPalette.Colors[0x30],
                };

                for ( int y = 0; y < 8 * 8; y += 8 )
                {
                    for ( int x = 0; x < 16 * 8; x += 8 )
                    {
                        DrawTile( reader, bmp, colors, x, y, true );
                    }
                }

                reader.BaseStream.Position = FontCHR - (TileSize * 6);

                // slash '/' isn't with the rest of the characters
                DrawTile( reader, bmp, colors, 15 * 8, 7 * 8, true );

                DrawExtraChars( bmp, colors[3] );

                Palette palette = new Palette( 0, 0x30, 0x10, 0 );

                DrawSprite( reader, EffectCHR, bmp, palette, 2, 2, 0, 8 * 8, true );

                bmp.MakeTransparent( DefaultSystemPalette.Colors[1] );
                bmp.Save( options.MakeOutPath( @"font.png" ), ImageFormat.Png );

                Bitmap bmpB = new Bitmap( bmp.Width, bmp.Height );

                using ( Graphics g = Graphics.FromImage( bmpB ) )
                {
                    g.DrawImage( bmp, 0, 0, new Rectangle( 0, 0, 10 * 8, 8 ), GraphicsUnit.Pixel );
                    for ( int i = 0; i < 10; i++ )
                    {
                        Outline( bmpB, new Rectangle( i * 8, 0, 8, 8 ) );
                    }

                    g.DrawImage( bmp, 80, 0, new Rectangle( 48, 8, 8, 8 ), GraphicsUnit.Pixel );
                    g.DrawImage( bmp, 88, 0, new Rectangle( 98, 16, 8, 8 ), GraphicsUnit.Pixel );
                    g.DrawImage( bmp, 92, 0, new Rectangle( 48, 24, 8, 8 ), GraphicsUnit.Pixel );
                    g.DrawImage( bmp, 100, 0, new Rectangle( 48, 24, 8, 8 ), GraphicsUnit.Pixel );
                    Outline( bmpB, new Rectangle( 80, 0, 32, 8 ) );
                }

                bmpB.Save( options.MakeOutPath( @"fontB.png" ), ImageFormat.Png );
            }
        }

        private static void DrawExtraChars( Bitmap bmp, Color color )
        {
            int glyphX = 0;
            int glyphY = 0;

            // colon in cell (15, 8)
            glyphX = 15 * 8;
            glyphY = 8 * 8;
            bmp.SetPixel( glyphX + 3, glyphY + 2, color );
            bmp.SetPixel( glyphX + 4, glyphY + 2, color );
            bmp.SetPixel( glyphX + 3, glyphY + 3, color );
            bmp.SetPixel( glyphX + 4, glyphY + 3, color );
            bmp.SetPixel( glyphX + 3, glyphY + 5, color );
            bmp.SetPixel( glyphX + 4, glyphY + 5, color );
            bmp.SetPixel( glyphX + 3, glyphY + 6, color );
            bmp.SetPixel( glyphX + 4, glyphY + 6, color );
        }

        static bool IsLetterColor( Color color )
        {
            return color.A == byte.MaxValue && color.ToArgb() != Color.Black.ToArgb();
        }

        private static void Outline( Bitmap bmp, Rectangle rect )
        {
            for ( int y = rect.Top; y < rect.Bottom; y++ )
            {
                for ( int x = rect.Left; x < rect.Right; x++ )
                {
                    if ( bmp.GetPixel( x, y ).A == byte.MaxValue )
                        continue;

                    bool draw = false;

                    draw = (x > rect.Left && IsLetterColor( bmp.GetPixel( x - 1, y ) ))
                        || (x < rect.Right - 1 && IsLetterColor( bmp.GetPixel( x + 1, y ) ))
                        || (y > rect.Top && IsLetterColor( bmp.GetPixel( x, y - 1 ) ))
                        || (y < rect.Bottom - 1 && IsLetterColor( bmp.GetPixel( x, y + 1 ) ));

                    if ( draw )
                        bmp.SetPixel( x, y, Color.Black );
                }
            }
        }

        static readonly Dictionary<int, int> OrigToNewMusic = new Dictionary<int, int>()
        {
            { 0x47, 6 },
            { 0x48, 7 },
            { 0x49, 8 },
            { 0x4a, 9 },
            { 0x4b, 10 },
            { 0x4c, 11 },
            { 0x4d, 12 },
            { 0x4e, 13 },
        };

        private static void ExtractLevelMusic( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                byte[] tileSetIDs = null;
                byte[] tilesetOrigMusic = null;
                byte[] levelMusic = new byte[64];

                reader.BaseStream.Position = TilesetIndexes;
                tileSetIDs = reader.ReadBytes( 64 );

                reader.BaseStream.Position = TilesetOrigMusic;
                tilesetOrigMusic = reader.ReadBytes( 8 );

                for ( int i = 0; i < 64; i++ )
                {
                    int tileset = tileSetIDs[i];
                    int origMusic = tilesetOrigMusic[tileset];

                    levelMusic[i] = (byte) OrigToNewMusic[origMusic];
                }

                File.WriteAllBytes( options.MakeOutPath( @"levelMusic.dat" ), levelMusic );
            }
        }

        private static void ExtractLevelTiles( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                HashSet<Tileset> tileSets = new HashSet<Tileset>();
                byte[] tileSetIDs = null;
                List<Tileset> graphicTileSets = new List<Tileset>();
                byte[] mapToGraphicTileset = new byte[64];

                reader.BaseStream.Position = TilesetIndexes;
                tileSetIDs = reader.ReadBytes( 64 );

                // there are really only 61 maps, but room is saved for 64
                for ( int i = 0; i < 61; i++ )
                {
                    reader.BaseStream.Position = LPalettes + i * 0x30;

                    Tileset tileset = new Tileset();
                    tileset.Pattern = tileSetIDs[i];
                    tileset.PaletteIndex = i * 3;
                    tileset.Palettes = new Palette[4];
                    for ( int j = 0; j < 4; j++ )
                    {
                        tileset.Palettes[j] = new Palette();
                        reader.Read( tileset.Palettes[j].Indexes, 0, 4 );
                    }
                    if ( tileSets.Add( tileset ) )
                        graphicTileSets.Add( tileset );
                    mapToGraphicTileset[i] = (byte) graphicTileSets.IndexOf( tileset );
                }

                File.WriteAllBytes( 
                    options.MakeOutPath( @"levelGraphicSets.dat" ), mapToGraphicTileset );

                int y = 0;

                for ( int i = 0; i < graphicTileSets.Count; i++ )
                {
                    var tileset = graphicTileSets[i];
                    using ( Bitmap bmp = new Bitmap( 16 * 16, 8 * 16 ) )
                    {
                        int paletteBase = LPalettes + tileset.PaletteIndex * 0x10;
                        int tileBuildBase = LTileBuild + tileset.Pattern * 0x200;
                        int chrBase = LTileCHR + tileset.Pattern * 0x800;
                        int attrBase = LTileAttr + tileset.Pattern * 0x80;

                        ExtractTiles( reader, paletteBase, tileBuildBase, attrBase, chrBase, bmp, y );
                        ExtractExtraTiles( reader, paletteBase, i, false, attrBase, chrBase, bmp, y );

                        string filename = string.Format( 
                            options.MakeOutPath( @"levelTilesOut{0:X2}.png" ), i );
                        bmp.Save( filename, ImageFormat.Png );
                    }
                }

                y = 0;

                for ( int i = 0; i < graphicTileSets.Count; i++ )
                {
                    var tileset = graphicTileSets[i];
                    using ( Bitmap bmp = new Bitmap( 16 * 16, 8 * 16 ) )
                    {
                        int paletteBase = LPalettes + (tileset.PaletteIndex + 2) * 0x10;
                        int tileBuildBase = LTileBuild + tileset.Pattern * 0x200;
                        int chrBase = LTileCHR + tileset.Pattern * 0x800;
                        int attrBase = LTileAttr + tileset.Pattern * 0x80;

                        ExtractTiles( reader, paletteBase, tileBuildBase, attrBase, chrBase, bmp, y );
                        ExtractExtraTiles( reader, paletteBase, i, true, attrBase, chrBase, bmp, y );

                        string filename = string.Format( 
                            options.MakeOutPath( @"levelTilesIn{0:X2}.png" ), i );
                        bmp.Save( filename, ImageFormat.Png );
                    }
                }
            }
        }

        private static void ExtractOverworldTileAttrs( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                reader.BaseStream.Position = OWTilesetAttr;
                byte[] attrBuf = reader.ReadBytes( 128 * 2 );
                File.WriteAllBytes( options.MakeOutPath( @"owTileAttr.dat" ), attrBuf );
            }
        }

        private static void ExtractOverworldMap( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                ushort[] rowPtrs = new ushort[256];

                reader.BaseStream.Position = OWMapRowTable;

                for ( int i = 0; i < rowPtrs.Length; i++ )
                {
                    rowPtrs[i] = reader.ReadUInt16();
                }

                int firstRowPos = (int) reader.BaseStream.Position;
                int lastRowOffset = rowPtrs[255] - rowPtrs[0];

                // look for the end of the last row

                reader.BaseStream.Position += lastRowOffset;

                while ( reader.ReadByte() != 0xff )
                {
                }

                // now that we know where all row data ends, read all row data

                int rowDataSize = (int) reader.BaseStream.Position - firstRowPos;

                reader.BaseStream.Position = firstRowPos;
                byte[] rowData = reader.ReadBytes( rowDataSize );

                string OWMapFile = options.MakeOutPath( @"owMap.tab" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( OWMapFile ) ) )
                {
                    for ( int i = 0; i < rowPtrs.Length; i++ )
                    {
                        // first byte of row data is offset 0
                        writer.Write( (ushort) (rowPtrs[i] - rowPtrs[0]) );
                    }

                    writer.Write( rowData );
                }
            }
        }

        private static void ExtractOverworldTiles( Options options )
        {
            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                Bitmap bmp = new Bitmap( 16 * 16, 8 * 16 );

                ExtractTiles( reader, OverworldPalettes, OWTileBuild, OWTileAttr, OWTileCHR, bmp, 0 );

                bmp.Save( options.MakeOutPath( @"owTiles.png" ), ImageFormat.Png );
            }
        }

        private static void ExtractExtraTiles(
            BinaryReader reader, int paletteBase, int tileset, bool inside,
            int attrBase, int chrBase, Bitmap bmp, int baseY )
        {
            byte[] buf = new byte[64];
            if ( inside )
            {
                // Tile data for two open chest bitmaps that I made based on the original closed ones.
                string resName;
                if ( tileset == 0x13 )
                    resName = "ExtractRes.Data.OpenChest2.bin";
                else
                    resName = "ExtractRes.Data.OpenChest1.bin";
                using ( var stream = GetResourceStream( resName ) )
                {
                    stream.Read( buf, 0, buf.Length );
                }
            }
            else
            {
                if ( tileset == 0 )
                    return;

                for ( int i = 0; i < buf.Length; i++ )
                    buf[i] = 0xFF;
            }
            MemoryStream memStream = new MemoryStream( buf );
            BinaryReader tileReader = new BinaryReader( memStream );

            reader.BaseStream.Position = paletteBase;

            Palette[] pals = new Palette[4];
            for ( int j = 0; j < 4; j++ )
            {
                pals[j] = new Palette();
                reader.Read( pals[j].Indexes, 0, 4 );
            }

            Color[] colors = new Color[4];

            for ( int j = 0; j < 4; j++ )
                colors[j] = DefaultSystemPalette.Colors[pals[0].Indexes[j]];

            DrawTile( tileReader, bmp, colors, 0xF0, 0x70 );
            DrawTile( tileReader, bmp, colors, 0xF0 + 8, 0x70 );
            DrawTile( tileReader, bmp, colors, 0xF0, 0x70 + 8 );
            DrawTile( tileReader, bmp, colors, 0xF0 + 8, 0x70 + 8 );
        }

        private static void ExtractTiles(
            BinaryReader reader, int paletteBase, int tileBuild,
            int attrBase, int chrBase, Bitmap bmp, int baseY )
        {
            reader.BaseStream.Position = paletteBase;

            Palette[] pals = new Palette[4];
            for ( int j = 0; j < 4; j++ )
            {
                pals[j] = new Palette();
                reader.Read( pals[j].Indexes, 0, 4 );
            }

            reader.BaseStream.Position = tileBuild;

            byte[] ul = reader.ReadBytes( 128 );
            byte[] ur = reader.ReadBytes( 128 );
            byte[] dl = reader.ReadBytes( 128 );
            byte[] dr = reader.ReadBytes( 128 );

            reader.BaseStream.Position = attrBase;

            byte[] attr = reader.ReadBytes( 128 );

            Color[] colors = new Color[4];
            int i = 0;

            for ( int y = 0; y < 8 * 16; y += 16 )
            {
                for ( int x = 0; x < bmp.Width; x += 16, i++ )
                {
                    int p = attr[i] & 3;

                    for ( int j = 0; j < 4; j++ )
                        colors[j] = DefaultSystemPalette.Colors[pals[p].Indexes[j]];

                    reader.BaseStream.Position = chrBase + ul[i] * TileSize;
                    DrawTile( reader, bmp, colors, x, baseY + y );

                    reader.BaseStream.Position = chrBase + ur[i] * TileSize;
                    DrawTile( reader, bmp, colors, x + 8, baseY + y );

                    reader.BaseStream.Position = chrBase + dl[i] * TileSize;
                    DrawTile( reader, bmp, colors, x, baseY + y + 8 );

                    reader.BaseStream.Position = chrBase + dr[i] * TileSize;
                    DrawTile( reader, bmp, colors, x + 8, baseY + y + 8 );
                }
            }
        }

        private static void ExtractMapObjects( Options options )
        {
            // Special handling for bats
            // Plain bats are found in 9 levels. They all behave the same, so they 
            // all use object ID 87. But they have different colors depending on the 
            // level they're found in. 
            // That was easy to do with palettes in the original game. Because palettes 
            // are not used in this port, each bat must be draw to the objects image in 
            // each color.
            // As part of this, assign new object IDs to the bats in the levels after 
            // the first with bats. They are assigned new IDs (208..215) based on the 
            // level they're found in (18, 22, 27..32).
            // Changing the object IDs also means that some entries in the map object 
            // table, which says which objects are made for each map, must be changed 
            // to reflect it.

            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                Palette[] pals = new Palette[2] { new Palette(), new Palette() };
                byte[] indexes = null;
                int[] objMaps = new int[208];
                List<int> batMaps = new List<int>();

                reader.BaseStream.Position = ObjectSpriteIndexes;
                indexes = reader.ReadBytes( 208 );

                reader.BaseStream.Position = MapObjects;

                for ( int i = 0; i < 64; i++ )
                {
                    reader.BaseStream.Position = MapObjects + i * 0x30;

                    for ( int j = 0; j < 0x10; j++, reader.BaseStream.Position += 2 )
                    {
                        byte objID = reader.ReadByte();

                        if ( objID == 0 )
                            break;

                        if ( objID == 0x57 && !batMaps.Contains( i ) )
                            batMaps.Add( i );

                        // These objects are in more than 1 place, but only the first is needed.
                        // Keep track of bats separately.
                        if ( (objID == 19 && i == 15)
                            || (objID == 22 && i == 48)
                            || (objID == 0x57 && i != batMaps[0]) )
                            continue;

                        objMaps[objID] = i;
                    }
                }

                // Bats in different maps look different. Assign the second and the rest new IDs
                // The original bat ID belongs to the bats in the first map that has them

                Array.Resize( ref indexes, 208 + batMaps.Count - 1 );
                Array.Resize( ref objMaps, 208 + batMaps.Count - 1 );
                for ( int i = 1; i < batMaps.Count; i++ )
                {
                    indexes[207 + i] = indexes[0x57];
                    objMaps[207 + i] = batMaps[i];
                }

                string ObjectsFile = options.MakeOutPath( @"objects.dat" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( ObjectsFile ) ) )
                {
                    writer.BaseStream.SetLength( 0 );

                    for ( int i = 0; i < 64; i++ )
                    {
                        reader.BaseStream.Position = MapObjects + i * 0x30;

                        for ( int j = 0; j < 0x10; j++ )
                        {
                            byte objID = reader.ReadByte();
                            byte objXAndFlags = reader.ReadByte();
                            byte objY = reader.ReadByte();

                            if ( objID == 0x57 )
                            {
                                int relMapIndex = batMaps.IndexOf( i );
                                if ( relMapIndex > 0 )
                                {
                                    objID = (byte) (207 + relMapIndex);
                                }
                            }

                            int flags = objXAndFlags & 0xc0;

                            if ( indexes[objID] == indexes[0x57] )
                                flags |= FlyerObjFlag;

                            writer.Write( objID );
                            writer.Write( (byte) (objXAndFlags & 0x3f) );
                            writer.Write( objY );
                            writer.Write( (byte) flags );
                        }
                    }
                }

                int y = 0;
                Bitmap bmp = new Bitmap( 4 * 16, (12 + 7) * 16 );
                Bitmap objBmp = new Bitmap( 4 * 16, indexes.Length * 16 );

                reader.BaseStream.Position = MapManMainColors;
                byte[] mapManMainColors = reader.ReadBytes( 12 * 2 );

                for ( int i = 0; i < 12; i++ )
                {
                    int spriteBase = MapManCHR + i * 0x100;

                    reader.BaseStream.Position = OverworldPalettes + 16;
                    reader.Read( pals[0].Indexes, 0, 4 );
                    reader.Read( pals[1].Indexes, 0, 4 );

                    pals[0].Indexes[2] = mapManMainColors[i * 2];
                    pals[1].Indexes[2] = mapManMainColors[i * 2 + 1];

                    DrawMapObjectSprite( reader, spriteBase + 0 * TileSize, bmp, pals, 0 * 16, y );
                    DrawMapObjectSprite( reader, spriteBase + 4 * TileSize, bmp, pals, 1 * 16, y );
                    DrawMapObjectSprite( reader, spriteBase + 8 * TileSize, bmp, pals, 2 * 16, y );
                    DrawMapObjectSprite( reader, spriteBase + 12 * TileSize, bmp, pals, 3 * 16, y );

                    y += 16;
                }

                for ( int i = 0; i < 7; i++ )
                {
                    int spriteBase = OWObjectCHR + i * 0x100;
                    int p = (i == 0) ? 1 : 0;

                    if ( i >= 5 )
                        spriteBase -= 4 * 4 * TileSize;
                    else if ( i >= 3 )
                        spriteBase -= 2 * 4 * TileSize;

                    reader.BaseStream.Position = OverworldPalettes + 24;
                    reader.Read( pals[0].Indexes, 0, 4 );
                    reader.Read( pals[1].Indexes, 0, 4 );

                    DrawSprite( reader, spriteBase + 0 * TileSize, bmp, pals[p], 2, 2, 0 * 16, y, true );
                    DrawSprite( reader, spriteBase + 4 * TileSize, bmp, pals[p], 2, 2, 1 * 16, y, true );
                    DrawSprite( reader, spriteBase + 8 * TileSize, bmp, pals[p], 2, 2, 2 * 16, y, true );
                    DrawSprite( reader, spriteBase + 12 * TileSize, bmp, pals[p], 2, 2, 3 * 16, y, true);

                    y += 16;
                }

                y = 0;

                for ( int i = 0; i < indexes.Length; i++ )
                {
                    int spriteBase = MapObjectCHR + indexes[i] * 0x100;

                    reader.BaseStream.Position = LPalettes + objMaps[i] * 0x30 + 24;
                    reader.Read( pals[0].Indexes, 0, 4 );
                    reader.Read( pals[1].Indexes, 0, 4 );

                    DrawMapObjectSprite( reader, spriteBase + 0 * TileSize, objBmp, pals, 0 * 16, y );
                    DrawMapObjectSprite( reader, spriteBase + 4 * TileSize, objBmp, pals, 1 * 16, y );
                    DrawMapObjectSprite( reader, spriteBase + 8 * TileSize, objBmp, pals, 2 * 16, y );
                    DrawMapObjectSprite( reader, spriteBase + 12 * TileSize, objBmp, pals, 3 * 16, y );

                    y += 16;
                }

                bmp.Save( options.MakeOutPath( @"mapPlayer.png" ), ImageFormat.Png );
                objBmp.Save( options.MakeOutPath( @"mapObjects.png" ), ImageFormat.Png );
            }
        }

        static void DrawMapObjectSprite(
            BinaryReader reader, int spriteBase, Bitmap bmp, Palette[] pals, int x, int y )
        {
            DrawSprite( reader, spriteBase, bmp, pals[0], 2, 1, x, y, true );
            DrawSprite( reader, spriteBase + 2 * TileSize, bmp, pals[1], 2, 1, x, y + 8, true );
        }

        private static void ExtractBattleSprites( Options options )
        {
            Palette[] palettes = null;
            byte[] palIndexes = null;

            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                palettes = ReadPalettes( reader, BattleSpritePalettes, 4 );

                reader.BaseStream.Position = BattleSpritePalIndexes;

                palIndexes = reader.ReadBytes( 12 );

                Bitmap bmp = new Bitmap( 16 * 8, 3 * 8 * 12 );
                Bitmap battleBmp = new Bitmap( 16 * 8, 2 * 8 * 64 );
                int y = 0;

                for ( int i = 0; i < 12; i++ )
                {
                    int spriteBase = BattleSpriteCHR + i * 0x200;
                    Palette pal = palettes[palIndexes[i]];

                    DrawSprite( reader, spriteBase, bmp, pal, 2, 3, 0, y, true );
                    DrawSprite( reader, spriteBase, bmp, pal, 2, 2, 2 * 8, y, true );
                    DrawSprite( reader, spriteBase + 6 * 16, bmp, pal, 2, 1, 2 * 8, y + 2 * 8, true );

                    DrawSprite( reader, spriteBase + 8 * 16, bmp, pal, 2, 3, 4 * 8, y, true );
                    DrawSprite( reader, spriteBase + 14 * 16, bmp, pal, 2, 3, 6 * 8, y, true );
                    DrawSprite( reader, spriteBase + 20 * 16, bmp, pal, 2, 3, 8 * 8, y, true );

                    DrawSprite( reader, spriteBase + 20 * 16, bmp, palettes[3], 2, 3, 10 * 8, y, true );

                    DrawSprite( reader, spriteBase + 26 * 16, bmp, pal, 3, 2, 12 * 8, y + 8, true );

                    y += 3 * 8;
                }

                Palette palette = new Palette();
                byte[] buf = new byte[8];

                y = 0;

                for ( int i = 0; i < 40; i++ )
                {
                    reader.BaseStream.Position = WeaponStats + i * 8;
                    reader.Read( buf, 0, 8 );

                    int spriteBase = WeaponCHR + (buf[6] - 0x80) * 16;

                    palette.Indexes[0] = 0x0f;
                    palette.Indexes[1] = buf[7];
                    palette.Indexes[2] = (byte) (buf[7] - 0x10);
                    palette.Indexes[3] = (byte) (buf[7] - 0x20);

                    DrawSprite( reader, spriteBase, battleBmp, palette, 2, 2, 0, y, true );

                    DrawSprite( reader, EffectCHR + 4 * 16, battleBmp, palette, 2, 2, 48, y, true );
                    DrawSprite( reader, EffectCHR + 8 * 16, battleBmp, palette, 2, 2, 64, y, true );
                    DrawSprite( reader, EffectCHR + 12 * 16, battleBmp, palette, 2, 2, 80, y, true );

                    y += 1 * 16;
                }

                byte[] bareStrikeColors = new byte[] { 0x28, 0x20, 0x24, 0x2c };

                // the punching arm
                for ( int i = 0; i < 4; i++ )
                {
                    int spriteBase = WeaponCHR + 44 * 16;

                    DrawSprite( reader, spriteBase, battleBmp, palettes[0], 2, 2, 0, y, true );

                    byte baseColor = bareStrikeColors[i];

                    palette.Indexes[0] = 0x0f;
                    palette.Indexes[1] = baseColor;
                    palette.Indexes[2] = (byte) (baseColor - 0x10);
                    palette.Indexes[3] = (byte) (baseColor - 0x20);

                    DrawSprite( reader, EffectCHR + 4 * 16, battleBmp, palette, 2, 2, 48, y, true );
                    DrawSprite( reader, EffectCHR + 8 * 16, battleBmp, palette, 2, 2, 64, y, true );
                    DrawSprite( reader, EffectCHR + 12 * 16, battleBmp, palette, 2, 2, 80, y, true );

                    y += 1 * 16;
                }

                y = 0;

                for ( int i = 0; i < 64; i++ )
                {
                    reader.BaseStream.Position = MagicStats + i * 8;
                    reader.Read( buf, 0, 8 );

                    int spriteBase = MagicCHR + (buf[5] - 0xB0) * 16;
                    int sprite2Base = spriteBase + 4 * TileSize;
                    byte pal = buf[6];

                    if ( pal == 0 )
                        pal = 0x10;

                    palette.Indexes[0] = 0x0f;
                    palette.Indexes[1] = pal;
                    palette.Indexes[2] = (byte) (pal - 0x10);
                    palette.Indexes[3] = 0x30;

                    DrawSprite( reader, spriteBase, battleBmp, palette, 2, 2, 16, y, true );
                    DrawSprite( reader, sprite2Base, battleBmp, palette, 2, 2, 32, y, true );

                    y += 1 * 16;
                }

                // finger cursor is among battle sprites - do it as part of menu

                bmp.Save( options.MakeOutPath( @"playerSprites.png" ), ImageFormat.Png );
                battleBmp.Save( options.MakeOutPath( @"battleSprites.png" ), ImageFormat.Png );
            }
        }

        static byte[] levelBackdrops = new byte[64]
        {
            0,
            3,
            0,
            0,
            0,
            0,
            12,
            0,
            9,
            9,
            9,
            9,
            5,
            8,
            14,
            13,
            2,
            2,
            15,
            2,
            2,
            2,
            1,
            11,
            9,
            9,
            9,
            1,
            1,
            8,
            8,
            8,
            8,
            14,
            14,
            14,
            14,
            13,
            13,
            2,
            11,
            11,
            12,
            12,
            12,
            12,
            12,
            11,
            11,
            11,
            11,
            11,
            5,
            5,
            5,
            5,
            5,
            5,
            5,
            5,
            2,
            0,
            0,
            0
        };

        private static void ExtractBackdrops( Options options )
        {
            Palette[] palettes = null;
            byte[] layout = null;

            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                palettes = ReadPalettes( reader, BackdropPal, 16 );

                reader.BaseStream.Position = BackdropLayout;

                layout = reader.ReadBytes( 14 );

                // backrops are 14x4 tiles each, and there are 16 of them
                Bitmap bmp = new Bitmap( 14 * 8, 4 * 8 * 16 );

                for ( int i = 0; i < 16; i++ )
                {
                    int chrBase = BattleCHRBase + (ChrSectionSize * i);
                    Color[] colors = new Color[]
                            {
                                DefaultSystemPalette.Colors[palettes[i].Indexes[0]],
                                DefaultSystemPalette.Colors[palettes[i].Indexes[1]],
                                DefaultSystemPalette.Colors[palettes[i].Indexes[2]],
                                DefaultSystemPalette.Colors[palettes[i].Indexes[3]],
                            };

                    for ( int j = 0; j < 14; j++ )
                    {
                        int tileAddr = chrBase + layout[j] * TileSize;

                        reader.BaseStream.Position = tileAddr;
                        DrawTile( reader, bmp, colors, j * 8, i * 32 );

                        reader.BaseStream.Position = tileAddr + (4 * TileSize);
                        DrawTile( reader, bmp, colors, j * 8, i * 32 + 8 );

                        reader.BaseStream.Position = tileAddr + (8 * TileSize);
                        DrawTile( reader, bmp, colors, j * 8, i * 32 + 16 );

                        reader.BaseStream.Position = tileAddr + (12 * TileSize);
                        DrawTile( reader, bmp, colors, j * 8, i * 32 + 24 );
                    }
                }

                bmp.Save( options.MakeOutPath( @"backdrops.png" ), ImageFormat.Png );
            }

            File.WriteAllBytes( options.MakeOutPath( @"levelBackdrops.dat" ), levelBackdrops );
        }

        struct Formation
        {
            public byte Type;
            public byte Pattern;
            public byte[] Ids;
            public byte[] MinMax;
            public byte SupriseRate;
            public byte Flags;
            public byte Song;
        }

        static readonly byte[][] LichPattern = 
        {
            new byte[] { 71,  72,  73,  74,  75,  0,   0,   0 },
            new byte[] { 76,  77,  78,  79,  80,  81,  0,   0 },
            new byte[] { 82,  83,  84,  85,  86,  87,  88,  0 },
            new byte[] { 89,  90,  91,  92,  93,  94,  95,  0 },
            new byte[] { 96,  97,  98,  99,  100, 101, 0,   0 },
            new byte[] { 102, 103, 104, 105, 106, 107, 108, 0 },
            new byte[] { 109, 110, 111, 112, 113, 114, 0,   0 },
            new byte[] { 115, 116, 117, 118, 119, 120, 121, 0 }
        };

        static readonly byte[][] KaliPattern = 
        {
            new byte[] { 17, 18, 19, 20, 21, 22, 23, 24 },
            new byte[] { 25, 26, 27, 28, 29, 30, 31, 0 },
            new byte[] { 32, 33, 34, 35, 36, 37, 38, 39 },
            new byte[] { 40, 41, 42, 43, 44, 45, 46, 47 },
            new byte[] { 48, 49, 50, 51, 52, 53, 54, 0 },
            new byte[] { 55, 56, 57, 58, 59, 60, 61, 0 },
            new byte[] { 0,  0,  62, 63, 64, 65, 66, 0 },
            new byte[] { 0,  0,  0,  67, 68, 69, 70, 0 }
        };

        static readonly byte[][] KrakenPattern = 
        {
            new byte[] { 0,  0,  18, 19, 20, 21, 0,  0 },
            new byte[] { 0,  22, 23, 24, 25, 26, 27, 0 },
            new byte[] { 0,  28, 29, 30, 31, 32, 33, 34 },
            new byte[] { 0,  35, 36, 37, 38, 39, 40, 41 },
            new byte[] { 0,  42, 43, 44, 45, 46, 47, 48 },
            new byte[] { 49, 50, 51, 52, 53, 54, 55, 56 },
            new byte[] { 0,  57, 58, 59, 60, 61, 62, 63 },
            new byte[] { 0,  64, 65, 66, 67, 68, 69, 0 }
        };

        static readonly byte[][] TiamatPattern = 
        {
            new byte[] { 0,   0,   0,   70,  71,  72,  0,   0 },
            new byte[] { 73,  74,  75,  76,  77,  78,  79,  0 },
            new byte[] { 80,  81,  82,  83,  84,  85,  86,  0 },
            new byte[] { 87,  88,  89,  90,  91,  92,  93,  0 },
            new byte[] { 94,  95,  96,  97,  98,  99,  100, 0 },
            new byte[] { 101, 102, 103, 104, 105, 106, 107, 108 },
            new byte[] { 0,   109, 110, 111, 112, 113, 114, 115 },
            new byte[] { 0,   116, 117, 118, 119, 120, 121, 0 }
        };

        static readonly byte[][] ChaosPattern = 
        {
            new byte[] { 0,  0,  0,  18,  19,  0,   20,  21,  22,  23,  0,   0,  0,  0 },
            new byte[] { 0,  0,  24, 25,  26,  27,  28,  29,  30,  31,  32,  0,  0,  0 },
            new byte[] { 0,  33, 34, 35,  36,  37,  38,  39,  40,  41,  42,  43, 0,  0 },
            new byte[] { 0,  44, 45, 46,  47,  48,  49,  50,  51,  52,  53,  54, 55, 0 },
            new byte[] { 56, 57, 58, 59,  60,  61,  62,  63,  64,  65,  66,  67, 68, 0  },
            new byte[] { 69, 70, 0,  71,  72,  73,  74,  75,  76,  77,  78,  79, 80, 81 },
            new byte[] { 82, 0,  0,  0,   83,  84,  85,  86,  87,  88,  0,   0,  0,  89 },
            new byte[] { 0,  0,  0,  0,   90,  91,  92,  93,  94,  95,  96,  0,  0,  0 },
            new byte[] { 0,  0,  0,  0,   97,  98,  99,  0,   100, 101, 102, 0,  0,  0 },
            new byte[] { 0,  0,  0,  103, 104, 105, 106, 107, 108, 109, 110, 0,  0,  0 },
            new byte[] { 0,  0,  0,  111, 112, 113, 0,   0,   0,   114, 115, 0,  0,  0 },
            new byte[] { 0,  0,  0,  0,   0,   0,   0,   0,   0,   116, 117, 0,  0,  0 },
        };

        static void DrawBoss(
            BinaryReader reader,
            Bitmap bmp,
            int tileSet,
            byte[][] tileIndexes,
            Palette palette,
            int x,
            int y )
        {
            Color[] colors = new Color[]
                                    {
                                        DefaultSystemPalette.Colors[palette.Indexes[0]],
                                        DefaultSystemPalette.Colors[palette.Indexes[1]],
                                        DefaultSystemPalette.Colors[palette.Indexes[2]],
                                        DefaultSystemPalette.Colors[palette.Indexes[3]],
                                    };
            int basePtr = BattleCHRBase + (tileSet * ChrSectionSize);

            for ( int r = 0; r < tileIndexes.Length; r++ )
            {
                for ( int c = 0; c < tileIndexes[0].Length; c++ )
                {
                    reader.BaseStream.Position = basePtr + tileIndexes[r][c] * TileSize;

                    DrawTile(
                        reader,
                        bmp,
                        colors,
                        x + c * 8,
                        y + r * 8,
                        true );
                }
            }
        }

        private static void ExtractEnemies( Options options )
        {
            Enemy[] enemies = new Enemy[128];
            string[] names = null;
            Palette[] palettes = null;
            Formation[] formations = new Formation[128];

            using ( BinaryReader reader = new BinaryReader( File.OpenRead( options.RomPath ) ) )
            {
                names = Text.ReadStringTable( reader, EnemyNamesBase, EnemyNamesPageBase, 128 );
                palettes = ReadPalettes( reader, BattlePalettesBase, 64 );

                reader.BaseStream.Position = FormationsBase;

                // for each formation
                for ( int i = 0; i < 128; i++ )
                {
                    byte typeAndPattern = reader.ReadByte();
                    byte pics = reader.ReadByte();

                    byte[] ids = reader.ReadBytes( 4 );
                    byte[] minMax = new byte[6];

                    reader.Read( minMax, 0, 4 );

                    byte[] paletteIds = reader.ReadBytes( 2 );

                    byte surpriseRate = reader.ReadByte();
                    byte paletteAssignAndNoRun = reader.ReadByte();

                    reader.Read( minMax, 4, 2 );

                    for ( int j = 0; j < 6; j++ )
                    {
                        int max = minMax[j] & 0xF;
                        if ( max == 0 )
                            continue;

                        int id = ids[j % 4];

                        int pic = (pics >> ((j % 4) * 2)) & 3;
                        pic = ((pic << 1) | (pic >> 1)) & 3;
                        enemies[id].Pic = pic;

                        int palIndex = paletteAssignAndNoRun >> 4;
                        palIndex = FlipNibble( palIndex );
                        palIndex = (palIndex >> (j % 4)) & 1;
                        enemies[id].Pal = paletteIds[palIndex];

                        enemies[id].Pattern = typeAndPattern & 0xF;

                        enemies[id].Name = names[id];
                    }

                    formations[i].Type = (byte) (typeAndPattern >> 4);
                    formations[i].Pattern = (byte) (typeAndPattern & 0xf);
                    formations[i].Ids = ids;
                    formations[i].MinMax = minMax;
                    formations[i].SupriseRate = surpriseRate;
                    formations[i].Flags = (byte) (paletteAssignAndNoRun & 0xf);
                    formations[i].Song = 0;
                }

                string FormationFile = options.MakeOutPath( @"formations.dat" );

                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( FormationFile ) ) )
                {
                    foreach ( Formation formation in formations )
                    {
                        writer.Write( formation.Type );
                        writer.Write( formation.Pattern );
                        writer.Write( formation.Ids );
                        writer.Write( formation.MinMax );
                        writer.Write( formation.SupriseRate );
                        writer.Write( formation.Flags );
                        writer.Write( formation.Song );
                        writer.Write( (byte) 0 );
                    }
                }

                string NamesFile = options.MakeOutPath( @"enemyNames.tab" );
                WriteStringTableFile( NamesFile, names );

                string EnemyAttrFile = options.MakeOutPath( @"enemyAttr.dat" );
                reader.BaseStream.Position = EnemyAttrs;
                byte[] enemyAttrBuf = reader.ReadBytes( 20 * 128 );
                File.WriteAllBytes( EnemyAttrFile, enemyAttrBuf );

                string SpecialNamesFile = options.MakeOutPath( @"specialNames.tab" );
                string[] specialNames = Text.ReadStringTable(
                    reader,
                    SpecialNamesBase,
                    SpecialNamesPageBase,
                    26 );
                WriteStringTableFile( SpecialNamesFile, specialNames );

                string AttackListsFile = options.MakeOutPath( @"attackLists.dat" );
                reader.BaseStream.Position = AttackLists;
                byte[] attackListBuf = reader.ReadBytes( 16 * 44 );
                File.WriteAllBytes( AttackListsFile, attackListBuf );

                string MagicAttrFile = options.MakeOutPath( @"magicAttr.dat" );
                reader.BaseStream.Position = MagicStats;
                byte[] magicAttrBuf = reader.ReadBytes( 8 * 64 );
                // Lock2 should be type 0xE, the same as Lock
                magicAttrBuf[8 * 23 + 4] = 0xE;
                // Tmpr should be type 0xB, similar but not the same type as Sabr
                magicAttrBuf[8 * 14 + 4] = 0xB;
                // Sabr should increase Hit Rate
                magicAttrBuf[8 * 54 + 0] = 40;
                // Soft should work in battle, use type 0x8 - remove status
                magicAttrBuf[8 * 40 + 4] = 8;
                magicAttrBuf[8 * 40 + 1] = 2;
                File.WriteAllBytes( MagicAttrFile, magicAttrBuf );

                string SpecialAttrFile = options.MakeOutPath( @"specialAttr.dat" );
                reader.BaseStream.Position = SpecialStats;
                byte[] specialAttrBuf = reader.ReadBytes( 8 * 26 );
                File.WriteAllBytes( SpecialAttrFile, specialAttrBuf );

                string WeaponAttrFile = options.MakeOutPath( @"weaponAttr.dat" );
                reader.BaseStream.Position = WeaponStats;
                byte[] weaponAttrBuf = reader.ReadBytes( 8 * 40 );
                File.WriteAllBytes( WeaponAttrFile, weaponAttrBuf );

                string ArmorAttrFile = options.MakeOutPath( @"armorAttr.dat" );
                reader.BaseStream.Position = ArmorStats;
                byte[] armorAttrBuf = reader.ReadBytes( 4 * 40 );
                File.WriteAllBytes( ArmorAttrFile, armorAttrBuf );

                string WeaponPermsFile = options.MakeOutPath( @"weaponPerms.dat" );
                reader.BaseStream.Position = WeaponPermissions;
                byte[] weaponPermsBuf = reader.ReadBytes( 2 * 40 );
                File.WriteAllBytes( WeaponPermsFile, weaponPermsBuf );

                string ArmorPermsFile = options.MakeOutPath( @"armorPerms.dat" );
                reader.BaseStream.Position = ArmorPermissions;
                byte[] armorPermsBuf = reader.ReadBytes( 2 * 40 );
                File.WriteAllBytes( ArmorPermsFile, armorPermsBuf );

                string ArmorTypesFile = options.MakeOutPath( @"armorTypes.dat" );
                reader.BaseStream.Position = ArmorTypes;
                byte[] armorTypesBuf = reader.ReadBytes( 1 * 40 );
                File.WriteAllBytes( ArmorTypesFile, armorTypesBuf );

                string MagicPermsFile = options.MakeOutPath( @"magicPerms.dat" );
                reader.BaseStream.Position = MagicPermissions;
                byte[] magicPermsBuf = reader.ReadBytes( 8 * 12 );
                File.WriteAllBytes( MagicPermsFile, magicPermsBuf );

                string XPFile = options.MakeOutPath( @"xp.dat" );
                reader.BaseStream.Position = LevelXP;
                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( XPFile ) ) )
                {
                    for ( int i = 0; i < 49; i++ )
                    {
                        int value = reader.ReadInt32();
                        value &= 0xffffff;
                        reader.BaseStream.Seek( -1, SeekOrigin.Current );
                        writer.Write( value );
                    }
                }

                string InitClassFile = options.MakeOutPath( @"initClass.dat" );
                reader.BaseStream.Position = ClassInitStats;
                byte[] initClassBuf = reader.ReadBytes( 16 * 6 );
                File.WriteAllBytes( InitClassFile, initClassBuf );

                // items, found money, magic names, class names
                string ItemNamesFile = options.MakeOutPath( @"itemNames.tab" );
                string[] itemNames = Text.ReadStringTable( 
                    reader, 
                    ItemNamesBase, 
                    ItemNamesPageBase, 
                    256 );
                WriteStringTableFile( ItemNamesFile, itemNames );

                string ItemTargetFile = options.MakeOutPath( @"itemTarget.dat" );
                byte[] compactItemTarget = new byte[(itemTarget.Length + 7) / 8];
                for ( int i = 0; i < itemTarget.Length; i++ )
                {
                    compactItemTarget[i / 8] |= (byte) (itemTarget[i] << (i % 8));
                }
                File.WriteAllBytes( ItemTargetFile, compactItemTarget );

                string MagicTargetFile = options.MakeOutPath( @"magicTarget.dat" );
                byte[] compactMagicTarget = new byte[(magicTarget.Length + 7) / 8];
                for ( int i = 0; i < magicTarget.Length; i++ )
                {
                    compactMagicTarget[i / 8] |= (byte) (magicTarget[i] << (i % 8));
                }
                File.WriteAllBytes( MagicTargetFile, compactMagicTarget );

                string NesPaletteFile = options.MakeOutPath( @"nesColors.dat" );
                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( NesPaletteFile ) ) )
                {
                    foreach ( var color in DefaultSystemPalette.Colors )
                    {
                        writer.Write( color.B );
                        writer.Write( color.G );
                        writer.Write( color.R );
                    }
                }

                int[] picUsed = new int[16 * 4];
                Bitmap[] bmps = new Bitmap[16];
                for ( int j = 0; j < 16; j++ )
                {
                    int w = 6 * 8 * 3;
                    int h = 4 * 8 * 2 + 6 * 8 * 2;
                    bmps[j] = new Bitmap( w, h );
                }

                string EnemyPosFile = options.MakeOutPath( @"enemyPos.dat" );
                using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( EnemyPosFile ) ) )
                {
                    for ( int i = 0; i < 128; i++ )
                    {
                        int tileSet = enemies[i].Pattern;
                        int offset = 0;
                        int rows = 0;
                        int cols = 0;
                        int pic = enemies[i].Pic;
                        int index = picUsed[tileSet * 4 + pic]++;

                        switch ( pic )
                        {
                        case 0: offset = 18; cols = 4; rows = 4; break;
                        case 1: offset = 18 + 16; cols = 4; rows = 4; break;
                        case 2: offset = 18 + 16 + 16; cols = 6; rows = 6; break;
                        case 3: offset = 18 + 16 + 16 + 36; cols = 6; rows = 6; break;
                        }

                        offset *= TileSize;

                        int x = index * cols * 8;
                        int y = 0;

                        if ( pic < 2 )
                            y = pic * rows * 8;
                        else
                            y = 2 * 4 * 8 + (pic - 2) * 6 * 8;

                        if ( tileSet == 0xD && pic == 0 )
                        {
                            cols = 8;
                            rows = 8;
                            x = 0;
                            y = rows * 8;
                            DrawBoss(
                                reader, bmps[tileSet], tileSet, KaliPattern, palettes[enemies[i].Pal],
                                x, y );
                        }
                        else if ( tileSet == 0xD && pic == 2 )
                        {
                            cols = 8;
                            rows = 8;
                            x = 0;
                            y = 0;
                            DrawBoss(
                                reader, bmps[tileSet], tileSet, LichPattern, palettes[enemies[i].Pal],
                                x, y );
                        }
                        else if ( tileSet == 0xE && pic == 1 )
                        {
                            cols = 8;
                            rows = 8;
                            x = 0;
                            y = 0;
                            DrawBoss(
                                reader, bmps[tileSet], tileSet, KrakenPattern,
                                palettes[enemies[i].Pal], x, y );
                        }
                        else if ( tileSet == 0xE && pic == 3 )
                        {
                            cols = 8;
                            rows = 8;
                            x = 0;
                            y = rows * 8;
                            DrawBoss(
                                reader, bmps[tileSet], tileSet, TiamatPattern,
                                palettes[enemies[i].Pal], x, y );
                        }
                        else if ( tileSet == 0xF && pic == 0 )
                        {
                            x = 0;
                            y = 0;
                            cols = 14;
                            rows = 12;
                            DrawBoss(
                                reader, bmps[tileSet], tileSet, ChaosPattern, palettes[enemies[i].Pal],
                                x, y );
                        }
                        else
                        {
                            DrawSprite(
                                reader,
                                BattleCHRBase + (tileSet * ChrSectionSize) + offset,
                                bmps[tileSet],
                                palettes[enemies[i].Pal],
                                cols, rows,
                                x, y, true );
                        }

                        writer.Write( (byte) x );
                        writer.Write( (byte) y );
                        writer.Write( (byte) (cols * 8) );
                        writer.Write( (byte) (rows * 8) );
                    }
                }

                for ( int j = 0; j < 16; j++ )
                {
                    string filename = string.Format( @"enemies{0:X}.png", j );
                    filename = options.MakeOutPath( filename );
                    bmps[j].Save( filename, ImageFormat.Png );
                }
            }
        }

        private static void WriteStringTableFile( string path, string[] strings )
        {
            using ( BinaryWriter writer = new BinaryWriter( File.OpenWrite( path ), Encoding.UTF8 ) )
            {
                writer.BaseStream.SetLength( 0 );
                Text.WriteStringTable( writer, strings );
            }
        }

        private static Palette[] ReadPalettes( BinaryReader reader, int pointer, int count )
        {
            reader.BaseStream.Position = pointer;

            Palette[] pals = new Palette[count];
            byte[] pal = new byte[4];

            for ( int i = 0; i < count; i++ )
            {
                reader.Read( pal, 0, 4 );

                pals[i] = new Palette();

                for ( int j = 0; j < 4; j++ )
                {
                    pals[i].Indexes[j] = pal[j];
                }
            }

            return pals;
        }

        private static void DrawSprite(
            BinaryReader reader,
            int pointer,
            Bitmap bmp,
            Palette palette,
            int cols,
            int rows,
            int x,
            int y,
            bool transparent = false )
        {
            reader.BaseStream.Position = pointer;

            Color[] colors = new Color[]
                    {
                        DefaultSystemPalette.Colors[palette.Indexes[0]],
                        DefaultSystemPalette.Colors[palette.Indexes[1]],
                        DefaultSystemPalette.Colors[palette.Indexes[2]],
                        DefaultSystemPalette.Colors[palette.Indexes[3]],
                    };

            for ( int j = 0; j < rows; j++ )
            {
                for ( int i = 0; i < cols; i++ )
                {
                    DrawTile( reader, bmp, colors, x + i * 8, y + j * 8, transparent );
                }
            }
        }

        private static void DrawTile( BinaryReader reader, Bitmap bmp, Color[] colors, int x, int y,
            bool transparent = false )
        {
            // read a whole tile's pixel data
            reader.Read( tileBuf, 0, tileBuf.Length );

            for ( int v = 0; v < 8; v++ )
            {
                for ( int u = 0; u < 8; u++ )
                {
                    int lo = (tileBuf[v] >> (7 - u)) & 1;
                    int hi = (tileBuf[v + 8] >> (7 - u)) & 1;
                    int pixel = lo | (hi << 1);
                    Color color = colors[pixel];

                    if ( pixel != 0 || !transparent )
                        bmp.SetPixel( x + u, y + v, color );
                }
            }
        }

        static int FlipNibble( int b )
        {
            return ((b & 1) << 3) | ((b & 2) << 1) | ((b & 4) >> 1) | ((b & 8) >> 3);
        }

        private static Stream GetResourceStream( string name )
        {
            var asm = System.Reflection.Assembly.GetExecutingAssembly();
            return asm.GetManifestResourceStream( name );
        }

        private static FileStream TruncateFile( string path )
        {
            return File.Open( path, FileMode.Create, FileAccess.Write );
        }
    }
}
