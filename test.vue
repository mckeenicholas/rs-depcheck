<template>
  <div class="flex flex-col items-center justify-center">
    <div v-if="isPending">
      <LoadingMessage message="Loading WCA Data" class="text-2xl m-4" />
    </div>

    <div v-else-if="isError || !data?.name" class="text-red-500">
      Error fetching data: {{ error?.message || "Unknown error occurred" }}
    </div>

    <template v-else>
      <h1 class="text-center text-2xl font-bold m-4">{{ data.name }}</h1>

      <div>
        <ControlPanel
          :event-ids="eventIds"
          v-model:selected-event-id="selectedEventId"
          v-model:sim-count="simCount"
          v-model:include-dnf="includeDnf"
          v-model:decay-rate="decayHalfLife"
          v-model:start-date="startDate"
          v-model:end-date="endDate"
          :disable-run="currentSelectedCompetitors.length < 2"
          @run-simulation="runSimulation"
        />

        <div
          v-if="!competitorsByEvent[selectedEventId]?.length"
          class="text-center m-6 text-lg"
        >
          No one is registered for this event
        </div>

        <ol
          v-else
          class="flex-1 rounded-md border overflow-y-auto"
          :class="{
            'max-h-[72vh]': width > BREAKPOINT,
            'max-h-[64vh]': width <= BREAKPOINT,
          }"
        >
          <li
            v-for="person in competitorsByEvent[selectedEventId]"
            :key="person.id"
            @click="toggleSelection(person)"
            class="p-2 hover:bg-secondary rounded-md flex justify-between items-center"
          >
            <span :class="{ 'text-muted-foreground': !person.selected }">
              <FlagIcon :code="person.country" :muted="!person.selected" />
              <a
                :href="`https://worldcubeassociation.org/persons/${person.id}?event=${selectedEventId}`"
                class="hover:underline ms-2"
                @click.stop
              >
                {{ person.name }}
              </a>
            </span>
            <Checkbox
              v-model:checked="person.selected"
              @click.stop
              class="me-2"
              aria-label="Select competitor {{ person.name }}"
            />
          </li>
        </ol>
      </div>
    </template>
  </div>
</template>