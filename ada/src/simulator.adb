-- A skeleton of an ADA program for an assignment in programming languages

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulator is

   ----GLOBAL VARIABLES---

   Liczba_Pracownikow  : constant Integer := 5;
   Liczba_Zestawow : constant Integer := 3;
   Liczba_Klientow  : constant Integer := 2;

   -- Reduce the factor for faster simulation, increase for slower
   Delay_Factor: constant Duration := 0.05;

   subtype Pracownicy is Integer range 1 .. Liczba_Pracownikow;
   subtype Zestawy is Integer range 1 .. Liczba_Zestawow;
   subtype Klienci is Integer range 1 .. Liczba_Klientow;

   --each Producer is assigned a Product that it produces
   -- 1x Burger = Hamburger, 3x Hamburger = Big Mac
   -- 1x Frytki - Male Frytki, 2x Frytki = Srednie Frytki, 3x - Duze Frytki
   -- 1x Cola - Mala Cola, 2x Cola - Srednia Cola, 3x - Duza Cola
   Nazwy_Skladnikow  : constant array (Pracownicy) of Unbounded_String :=
     (To_Unbounded_String("Burger"), To_Unbounded_String("Frytki"), To_Unbounded_String("Cola"), To_Unbounded_String("Gwiazdki serowe"), To_Unbounded_String("6x Nuggetsy"));

   -- Zestaw sklada sie z kilku produktow
   Nazwy_Zestawow : constant array (Zestawy) of Unbounded_String :=
     (To_Unbounded_String("McZestaw Big Mac"), To_Unbounded_String("Zestaw Bambi"), To_Unbounded_String("Powiekszony McZestaw Nuggets + Hamburger"));


   ----TASK DECLARATIONS----

   -- Producer produces determined product
   task type Pracownik is
      entry Start (Product : in Pracownicy; Production_Time : in Integer);
   end Pracownik;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   -- but he/she orders it randomly
   task type Klient is
      entry Start
        (Consumer_Number : in Klienci; Consumption_Time : in Integer);
   end Klient;

   task Cleaning is
        entry Start(Cleaning_Day_When: in Integer);
    end Cleaning;

   -- Buffer receives products from Producers and delivers Assemblies to Consumers
   task type Buffer is
      -- Accept a product to the storage (provided there is a room for it)
      entry Take (Product : in Pracownicy; Number : in Integer);
      -- Deliver an assembly (provided there are enough products for it)
      entry Deliver (Assembly : in Zestawy; Number : out Integer);
      -- Remove some products from storage
      entry Cleaning_Day;
      -- Print hit/miss stats of Take and Deliver to a file
      entry Print_Stats;
      -- Check if we can safely accept more products
      entry Check_Buffer_Threshold(Product: in Pracownicy; Can_Accept: out Boolean);
   end Buffer;

   P : array (1 .. Liczba_Pracownikow) of Pracownik;
   K : array (1 .. Liczba_Klientow) of Klient;
   B : Buffer;

   ----TASK DEFINITIONS----

   --Producer--

   task body Pracownik is
      subtype Production_Time_Range is Integer range 1 .. 3;
      package Random_Production is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      --  random number generator
      G                    : Random_Production.Generator;
      Producer_Type_Number : Integer;
      Product_Number       : Integer;
      Production           : Integer;
      Random_Time          : Duration;
   begin
      accept Start (Product : in Pracownicy; Production_Time : in Integer)
      do
         --  start random number generator
         Random_Production.Reset (G);
         Product_Number       := 1;
         Producer_Type_Number := Product;
         Production           := Production_Time;
      end Start;
      Put_Line
        (ESC & "[93m" & "P: Started producer of " &
         To_String(Nazwy_Skladnikow (Producer_Type_Number)) & ESC & "[0m");
      loop
         --  introducing local scope
         declare
            Can_Safely_Accept: Boolean;
         begin
            B.Check_Buffer_Threshold(Producer_Type_Number, Can_Safely_Accept);
            Random_Time := Duration(Random_Production.Random(G));
            if not Can_Safely_Accept then
               --  delay production
               Random_Time := Random_Time * 2;
               Put_Line(ESC & "[93m" & "P: Demand for " & To_String(Nazwy_Skladnikow(Producer_Type_Number)) & " is low delaying production" & ESC & "[0m");
            end if;
         end;
         --  end of local scope
         delay Random_Time * Delay_Factor;
         Put_Line
           (ESC & "[93m" & "P: Produced product " &
            To_String(Nazwy_Skladnikow (Producer_Type_Number)) & " number " &
            Integer'Image (Product_Number) & ESC & "[0m");
         -- Accept for storage
         loop
            select
               B.Take(Producer_Type_Number, Product_Number);
               Product_Number := Product_Number + 1;
               exit;
            else
               Put_Line("Other product is being loaded to buffer. PLZ WAIT");
               delay 0.5 * Delay_Factor;
            end select;
         end loop;
      end loop;
   end Pracownik;

   --Consumer--

   task body Klient is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random
        (Consumption_Time_Range);

      --each Consumer takes any (random) Assembly from the Buffer
      package Random_Assembly is new Ada.Numerics.Discrete_Random
        (Zestawy);

      G               : Random_Consumption.Generator;
      GA              : Random_Assembly.Generator;
      Consumer_Nb     : Klienci;
      Assembly_Number : Integer;
      Consumption     : Integer;
      Assembly_Type   : Integer;
      Consumer_Name   :
        constant array (1 .. Liczba_Klientow) of String (1 .. 9) :=
        ("Consumer1", "Consumer2");
   begin
      accept Start
        (Consumer_Number : in Klienci; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);
         Random_Assembly.Reset (GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line
        (ESC & "[96m" & "C: Started consumer " & Consumer_Name (Consumer_Nb) &
         ESC & "[0m");
      loop
         delay Duration
           (Random_Consumption.Random (G) * Delay_Factor); --  simulate consumption
         Assembly_Type := Random_Assembly.Random (GA);
         -- take an assembly for consumption
         B.Deliver (Assembly_Type, Assembly_Number);
         -- if the assembly is not available, print a message
         if Assembly_Number = 0 then
             Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " lacks assembly of type " & To_String(Nazwy_Zestawow(Assembly_Type)) & ESC & "[0m");
         else
             Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) &
                      " takes assembly " & To_String(Nazwy_Zestawow(Assembly_Type)) & " number " &
                      Integer'Image(Assembly_Number) & ESC & "[0m");
         end if;
      end loop;
   end Klient;

   -- Cleaning
   task body Cleaning is
       Cleaning_Day: Integer;
       Day_Number: Integer := 1;
       Day_Duration: constant Duration := 2.0;
   begin
      -- set on which day the cleaning will be
       accept Start(Cleaning_Day_When: in Integer) do
           Cleaning_Day := Cleaning_Day_When;
           Put_Line(ESC & "[92m" & "Cleaning countdown started" & ESC & "[0m");
       end Start;
       loop
            -- wait for day to end
            delay Day_Duration * Delay_Factor;
            Put_Line (ESC & "[92m" & "Day" & Integer'Image(Day_Number) & ESC & "[0m");
            if Day_Number mod Cleaning_Day = 0 then
               -- Time to clean
               Put_Line (ESC & "[92m" & "Cleaning day" & ESC & "[0m");
               B.Cleaning_Day;
            end if;
            Day_Number := Day_Number + 1;
       end loop;
   end Cleaning;


   --Buffer--

   task body Buffer is
      --  number 8 is temporary, should be calculated based on products needs
      --  we need to find the sweet spot for the storage capacity and the safe thresholds
      Storage_Capacity : constant Integer := 50;
      type Stat is (Hit, Miss);
      type Storage_type is array (Pracownicy) of Integer;
      Storage              : Storage_type := (0, 0, 0, 0, 0);
      Zawartosc_Zestawow     : array (Zestawy, Pracownicy) of Integer :=
            ((3, 2, 2, 0, 0), (0, 1, 0, 3, 1), (1, 3, 3, 0, 4));
      Max_Assembly_Content : array (Pracownicy) of Integer;
      Assembly_Number      : array (Zestawy) of Integer := (1, 1, 1);
      In_Storage           : Integer := 0;

      Thresholds : array (Pracownicy) of Integer;

      -- Stats store counters for hits (successful Take/Deliver) and misses (rejected Take/Deliver)
      type Stat_Counter is delta 0.01 digits 20;
      -- Stats for Take
      Product_Stats : array(Stat) of Stat_Counter := (0.0, 0.0);
      -- Stats for Deliver
      Assembly_Stats: array(Stat) of Stat_Counter := (0.0, 0.0);
      Stat_File: Ada.Text_IO.File_Type;
      
      -- Priority is calculated as the difference between the maximum assembly content and the current storage
      -- We care less about products that can be already assembled
      Priority: array(Pracownicy) of Integer := (3, 3, 3, 3, 3);
      Lowest_Priority_Producer: Pracownicy;

     -- Recalculate priority for all producers, find the one with the lowest priority
      procedure RecalculatePriority is
        Lowest_Priority: Integer := 999999;
      begin
        for P in Pracownicy loop
            Priority(P) := Max_Assembly_Content(P) - Storage(P);
            if Priority(P) < Lowest_Priority then
                Lowest_Priority := Priority(P);
                Lowest_Priority_Producer := P;
            end if;
        end loop;
      end RecalculatePriority;

      procedure Setup_Variables is
         Total_Expected_Demand: Integer := 0;
         Multiplier: Float;
         Theoretically_Perfect_Buffer_Size: Integer;
      begin
         for W in Pracownicy loop
            Max_Assembly_Content (W) := 0;
            for Z in Zestawy loop
               if Zawartosc_Zestawow (Z, W) > Max_Assembly_Content (W) then
                  Max_Assembly_Content (W) := Zawartosc_Zestawow (Z, W);
               end if;
            end loop;
            Thresholds(W) := Max_Assembly_Content(W) * 2;
         end loop;
      
         declare
            Expected_Demand: array(Pracownicy) of Integer;
         begin
            for W in Pracownicy loop
               Expected_Demand(W) := 0;
               for Z in Zestawy loop
                  Expected_Demand(W) := Expected_Demand(W) + Zawartosc_Zestawow(Z, W);
               end loop;
               Total_Expected_Demand := Total_Expected_Demand + Expected_Demand(W);
            end loop;
      
            --  Determine the theoretically perfect buffer size
            Theoretically_Perfect_Buffer_Size := ((Integer(Float(Total_Expected_Demand) * 2.0) + 9) / 10) * 10;
            if Storage_Capacity < Theoretically_Perfect_Buffer_Size then
               Put_Line(ESC & "[91m" & "|   " & "Warning: Buffer size" & Integer'Image(Storage_Capacity) & " is not optimal, should be" & Integer'Image(Theoretically_Perfect_Buffer_Size) & ESC & "[0m");
            end if;

            -- Set the thresholds based on the multiplier
            Multiplier := (Float(Storage_Capacity) / Float(Total_Expected_Demand)) * 1.5;
            for W in Pracownicy loop
               Thresholds(W) := Integer(Float(Expected_Demand(W)) * Multiplier);
               Put_Line(ESC & "[91m" & "|   " & "Threshold for Pracownik" & Integer'Image(W) & ": " & Integer'Image(Thresholds(W)) & ESC & "[0m");
            end loop;

         end;
      end Setup_Variables;

      procedure Today_Is_Cleaning_Day is
        Cleaning_Day_Takes: constant Integer := 3;
      begin
        for P in Pracownicy loop
            if Storage(P) >= Cleaning_Day_Takes then
                Put_Line ( ESC & "[92m" & "Cleaning storage for product " & Integer'Image(P) & ESC & "[0m");
                Storage(P) := Storage(P) - Cleaning_Day_Takes;
                In_Storage := In_Storage - Cleaning_Day_Takes;
            end if;
        end loop;
      end;

      function Find_Max_In_Storage return Pracownicy is
      begin
        return Lowest_Priority_Producer;
      end Find_Max_In_Storage;
      
      procedure RemoveItem(Producer: Pracownicy) is
      begin
        if Storage(Producer) = 0 then
            return;
        end if;
        Storage(Producer) := Storage(Producer) - 1;
        In_Storage := In_Storage - 1;
      end;

      -- Check if we should cleanup redundant the storage
      function ShouldCleanup return Boolean is
        begin
            return In_Storage >= Storage_Capacity;
        end;

      -- Remove the product with the lowest priority
      function CleanupRedundantStorage(Producer: Pracownicy) return Pracownicy is
      begin
        RemoveItem(Find_Max_In_Storage);
        RecalculatePriority;
        return Find_Max_In_Storage;
      end;

      function Can_Accept (Product : Pracownicy) return Boolean is
      begin
       return In_Storage < Storage_Capacity;
      end Can_Accept;

      function Can_Safely_Accept(Product: Pracownicy) return Boolean is
      begin
         if Storage(Product) < Thresholds(Product) then
            return True;
         else
            return False;
         end if;
      end Can_Safely_Accept;

      function Can_Deliver (Assembly : Zestawy) return Boolean is
      begin
         for W in Pracownicy loop
            if Storage (W) < Zawartosc_Zestawow (Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Pracownicy loop
            Put_Line
              ("|   Storage contents: " & Integer'Image (Storage (W)) & " " &
               To_String(Nazwy_Skladnikow (W)));
         end loop;
         Put_Line
           ("|   Number of products in storage: " &
            Integer'Image (In_Storage));
      end Storage_Contents;

   begin
      Create (Stat_File, Ada.Text_IO.Out_File, "stats.txt");
      Close(Stat_File);
      Put_Line (ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
      Setup_Variables;
      loop
      select
         accept Take (Product : in Pracownicy; Number : in Integer) do
            -- check if there is a room for the product, if there is no room, remove redundant product, don't go inside if we just removed the current product to avoid filling it up again
            if not (ShouldCleanup and then CleanupRedundantStorage(Product) = Product) and Can_Accept (Product) then
               Put_Line
                 (ESC & "[91m" & "B: Accepted product " &
                  To_String(Nazwy_Skladnikow (Product)) & " number " &
                  Integer'Image (Number) & ESC & "[0m");
               Storage (Product) := Storage (Product) + 1;
               In_Storage        := In_Storage + 1;
               Product_Stats(Hit) := Product_Stats(Hit) + 1.0;
               RecalculatePriority;
            else
               Put_Line
                 (ESC & "[91m" & "B: Rejected product " &
                  To_String(Nazwy_Skladnikow (Product)) & " number " &
                  Integer'Image (Number) & ESC & "[0m");
                Product_Stats(Miss) := Product_Stats(Miss) + 1.0;
            end if;
         end Take;
         Storage_Contents;
        or
         accept Deliver (Assembly : in Zestawy; Number : out Integer) do
            if Can_Deliver (Assembly) then
               Put_Line
                 (ESC & "[91m" & "B: Delivered assembly " &
                  To_String(Nazwy_Zestawow (Assembly)) & " number " &
                  Integer'Image (Assembly_Number (Assembly)) & ESC & "[0m");
               for W in Pracownicy loop
                  Storage (W) := Storage (W) - Zawartosc_Zestawow (Assembly, W);
                  In_Storage  := In_Storage - Zawartosc_Zestawow (Assembly, W);
               end loop;
               Number                     := Assembly_Number (Assembly);
               Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
               Assembly_Stats(Hit) := Assembly_Stats(Hit) + 1.0;
            else
               Put_Line
                 (ESC & "[91m" & "B: Lacking products for assembly " &
                  To_String(Nazwy_Zestawow (Assembly)) & ESC & "[0m");
               Number := 0;
                Assembly_Stats(Miss) := Assembly_Stats(Miss) + 1.0;
            end if;
         end Deliver;
         Storage_Contents;
        or
            accept Cleaning_Day do
                Today_Is_Cleaning_Day;
            end Cleaning_Day;
            Storage_Contents;
        or
            accept Print_Stats do
                Open(Stat_File, Ada.Text_IO.Append_File, "stats.txt");
                Put_Line (Stat_File, "Product stats:");
                Put_Line (Stat_File, "Hits: " & Stat_Counter'Image(Product_Stats(Hit)));
                Put_Line (Stat_File, "Misses: " & Stat_Counter'Image(Product_Stats(Miss)));
                Put_Line (Stat_File, "Ratio: " & Stat_Counter'Image(Product_Stats(Hit) / (Product_Stats(Hit) + Product_Stats(Miss))));
                Put_Line (Stat_File, "Assembly stats:");
                Put_Line (Stat_File, "Hits: " & Stat_Counter'Image(Assembly_Stats(Hit)));
                Put_Line (Stat_File, "Misses: " & Stat_Counter'Image(Assembly_Stats(Miss)));
                Put_Line (Stat_File, "Ratio: " & Stat_Counter'Image(Assembly_Stats(Hit) / (Assembly_Stats(Hit) + Assembly_Stats(Miss))));
                Close(Stat_File);
            end Print_Stats;
         or
            accept Check_Buffer_Threshold(Product: in Pracownicy; Can_Accept: out Boolean) do
               Can_Accept := Can_Safely_Accept(Product);
            end Check_Buffer_Threshold;
        end select;
      end loop;
   end Buffer;

   ---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Liczba_Pracownikow loop
      P (I).Start (I, 10);
   end loop;
   for J in 1 .. Liczba_Klientow loop
      K (J).Start (J, 12);
   end loop;
    Cleaning.Start(10);
    loop
        delay 10.0;
        B.Print_Stats;
    end loop;
end Simulator;

