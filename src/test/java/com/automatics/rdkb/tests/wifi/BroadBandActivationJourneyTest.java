/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.wifi;

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.enums.BroadBandActivationEventEnum;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandActivationEventUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.exceptions.TestException;

/**
 * Test Class that contains the test cases/ methods for validating the Activation Journey including various events with
 * Log Messages.
 * 
 * @author BALAJI V
 * @refactor yamini.s
 **/

public class BroadBandActivationJourneyTest extends AutomaticsTestBase {

    /**
     *
     * Test Case # 1: Verify the various events in the activation journey are captured and the time required for each of
     * the events in the devices.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify the telemetry log message for DOCSIS registration completion.</li>
     * <li>S2) Verify the telemetry log message for LAN Initialization Start.</li>
     * <li>S3) Verify the telemetry log message for LAN Initialization completion.</li>
     * <li>S4) Verify the telemetry log message for WAN Initialization Start.</li>
     * <li>S5) Verify the telemetry log message for WAN Initialization completion.</li>
     * <li>S6) Verify the telemetry log message for Client connection completion</li>
     * <li>S7) Verify the telemetry log message for Broadcast for the Wifi Name (2.4GHz).</li>
     * <li>S8) Verify the telemetry log message for Broadcast for the Wifi Name (5GHz).</li>
     * <li>S9) Verify the telemetry log message for Wifi Broadcast completion.</li>
     * <li>S10) Verify the telemetry log message for entering Wifi Personalization Captive Mode.</li>
     * <li>S11) Verify the telemetry log message for exiting Wifi Personalization Captive Mode.</li>
     * </ol>
     *
     * @author BALAJI V
     * 
     * @param device
     *            {@link Dut}
     * @refactor Yamini.s
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-ACTV-TMTRY-5001")
    public void testBroadBandActivationJourney(Dut device) {
	String testCaseId = "TC-RDKB-ACTV-TMTRY-501";
	boolean result = false;
	String step = null;
	String errorMessage = null;
	try {
	    String response = null;
	    String parameterName = null;
	    int stepNumber = 0;
	    stepNumber++;
	    step = "S" + stepNumber;
	    result = false;

	    LOGGER.info("### PRE-CONDITION ### BEGIN FACTORY RESET OPERATION ON THE DEVICE.");
	    if (!(BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device)
		    || BroadBandCommonUtils.performFactoryResetSnmp(tapEnv, device))) {
		errorMessage = "Device Factory Reset could not be performed as a part of pre-condition.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("### PRE-CONDITION ### END FACTORY RESET OPERATION ON THE DEVICE.");

	    boolean isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    LOGGER.info("IS ATOM SYNC AVAILABLE: " + isAtomSyncAvailable);

	    String wifi1Ssid = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID);
	    LOGGER.info("2.4GHz PRIVATE SSID VALUE: " + wifi1Ssid);
	    String wifi2Ssid = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID);
	    LOGGER.info("5GHz PRIVATE SSID VALUE: " + wifi2Ssid);

	    // List to hold the events to be validated during each iteration.
	    List<BroadBandActivationEventEnum> activationEventsValidated = BroadBandActivationEventUtils
		    .getEventsToBeValidated(device.getModel(), wifi1Ssid, wifi2Ssid);
	    // Start capturing the activation events.
	    // Loop through each of the activation events and get the activation event logs until all the activation
	    // events have been populated with respective log messages or the poll duration lapses.
	    long startTime = System.currentTimeMillis();
	    boolean breakIteration = false;
	    do {
		breakIteration = true;
		tapEnv.waitTill(BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
		for (BroadBandActivationEventEnum activationEvent : activationEventsValidated) {
		    if (activationEvent.getLogSearchResponse().equals(BroadBandTestConstants.EMPTY_STRING)) {
			breakIteration = false;
			response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
				activationEvent.getLogMessageToBeSearched(), activationEvent.getLogFileName());
			result = CommonMethods.isNotNull(response);
			if (result) {
			    activationEvent.setLogSearchResponse(response);
			}
		    }
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
		    && !breakIteration);

	    /**
	     * S1) Verify the telemetry log message for DOCSIS registration completion.
	     * 
	     * S2) Verify the telemetry log message for LAN Initialization Start.
	     * 
	     * S3) Verify the telemetry log message for LAN Initialization completion.
	     * 
	     * S4) Verify the telemetry log message for WAN Initialization Start
	     * 
	     * S5) Verify the telemetry log message for WAN Initialization completion.
	     * 
	     * S6) Verify the telemetry log message for Client connection completion.
	     * 
	     * S7) Verify the telemetry log message for Broadcast for the Wifi Name (2.4GHz).
	     * 
	     * S8) Verify the telemetry log message for Broadcast for the Wifi Name (5GHz).
	     * 
	     * S9) Verify the telemetry log message for Wifi Broadcast completion.
	     * 
	     * S10) Verify the telemetry log message for entering Wifi PersonalizationCaptive Mode.
	     */
	    if (DeviceModeHandler.isDSLDevice(device) || CommonMethods.isRunningEthwanMode()) {
		errorMessage = "DOCSIS is not applicable for DSL device and Ethwan.";
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
		stepNumber++;
		step = "S" + stepNumber;
	    }
	    
	    LOGGER.info("STARTING TESTCASE EXECUTION :TC-RDKB-ACTV-TMTRY-5001");
	    String partnerId = null;
	    partnerId = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
	    LOGGER.info("PARTNER ID :"+partnerId);
	    for (BroadBandActivationEventEnum activationEvent : activationEventsValidated) {
		parameterName = activationEvent.getLogMessageToBeSearched();
		LOGGER.info(step + ") VERIFY CUSTOMER ACTIVATION JOURNEY - " + parameterName);
		LOGGER.info(step + " - EXPECTED - LOG MESSAGE MUST BE PRESENT FOR PARAMETER: " + parameterName);
		response = activationEvent.getLogSearchResponse();
		LOGGER.info("SEARCH RESPONSE: " + response);
		result = CommonMethods.isNotNull(response);
		errorMessage = "No log messages available for activation parameter: " + parameterName;
		if (BroadBandCommonUtils.patternSearchFromTargetString(activationEvent.getLogMessageToBeSearched(),
			BroadBandTraceConstants.ACTIVATION_ENTER_WIFI_PERSONALIZATION_CAPTIVE)
			&& partnerId.equalsIgnoreCase(
				BroadbandPropertyFileHandler.getCaptivePortalDefaultDisabledPartner())) {
		    LOGGER.info("EXITING VALIDATION SINCE ITS A "
			    + BroadbandPropertyFileHandler.getCaptivePortalDefaultDisabledPartner() + " DEVICE");
		    errorMessage = "wifi captive portal has been disabled by default for "
			    + BroadbandPropertyFileHandler.getCaptivePortalDefaultDisabledPartner();
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		} else {
		    if (result && !activationEvent.getLogMessageToBeSearched()
			    .contains(BroadBandTraceConstants.ACTIVATION_WIFI_NAME_BROADCAST)) {
			response = BroadBandActivationEventUtils.extractValueActivationJourney(response,
				activationEvent.getLogMessageToBeSearched());
			result = BroadBandCommonUtils.compareValues("INT_UNSIGNED", BroadBandTestConstants.STRING_ZERO,
				response);
			errorMessage = "Invalid Uptime associated with parameter: " + parameterName;
		    }
		    LOGGER.info(step + " - ACTUAL: "
			    + (result ? "CUSTOMER ACTIVATION JOURNEY - " + parameterName + " VERIFIED SUCCESSFULLY."
				    : errorMessage));
		    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

		    result = false;
		}
		stepNumber++;
		step = "S" + stepNumber;
	    }
	    if (DeviceModeHandler.isDSLDevice(device)) {
		errorMessage = "CAPTIVE MODE is not applicable for DSL devices.";
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
		stepNumber++;
		step = "S" + stepNumber;
	    }

	    /**
	     * S11) Verify the telemetry log message for exiting Wifi Personalization Captive Mode.
	     */
	    if (!DeviceModeHandler.isDSLDevice(device)) {
		parameterName = BroadBandTraceConstants.ACTIVATION_EXIT_WIFI_PERSONALIZATION_CAPTIVE;
		LOGGER.info(step + ") VERIFY CUSTOMER ACTIVATION JOURNEY - EXIT WIFI PERSONALIZATION CAPTIVE MODE.");
		LOGGER.info(step + " - EXPECTED - LOG MESSAGE MUST BE PRESENT FOR PARAMETER: " + parameterName);
		result = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_INFO_RDK_CENTRAL_CONFIGURE_WIFI,
			BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE);
		LOGGER.info("SETTING WIFI CONFIG STATUS: " + result);
		errorMessage = "Unable to configure Wifi Configuration Parameter to FALSE.";
		long pollDuration = 0L;
		if (result) {
		    pollDuration = BroadBandTestConstants.ONE_MINUTE_IN_MILLIS;
		    startTime = System.currentTimeMillis();
		    do {
			LOGGER.info("GOING TO WAIT FOR 5 SECONDS.");
			tapEnv.waitTill(BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
			response = isAtomSyncAvailable
				? BroadBandCommonUtils.searchLogFiles(tapEnv, device, parameterName,
					BroadBandTestConstants.RDKLOGS_LOGS_ARM_CONSOLE_0)
				: BroadBandCommonUtils.searchLogFiles(tapEnv, device, parameterName,
					BroadBandTestConstants.RDKLOGS_LOGS_CONSOLE_TXT_0);
			result = CommonMethods.isNotNull(response);
		    } while ((System.currentTimeMillis() - startTime) < pollDuration && !result);
		}
		errorMessage = "No log messages available for activation parameter: " + parameterName;
		if (result) {
		    response = BroadBandActivationEventUtils.extractValueActivationJourney(response, parameterName);
		    result = BroadBandCommonUtils.compareValues("INT_UNSIGNED", "0", response);
		    errorMessage = "Invalid Uptime associated with parameter: " + parameterName;
		}
		LOGGER.info(step + " - ACTUAL: " + (result
			? "CUSTOMER ACTIVATION JOURNEY - EXIT WIFI PERSONALIZATION CAPTIVE MODE VERIFIED SUCCESSFULLY."
			: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = "CAPTIVE MODE is not applicable for DSL devices.";
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE CUSTOMER ACTIVATION JOURNEY: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {

	    BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

	    LOGGER.info("### POST-CONDITION ### BEGIN RE-ACTIVATION OPERATION ON THE DEVICE.");
	    BroadBandWiFiUtils.reactivateDeviceUsingWebpaOrSnmp(tapEnv, device);
	    LOGGER.info("### POST-CONDITION ### END RE-ACTIVATION OPERATION ON THE DEVICE.");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-ACTV-TMTRY-5001");
    }
    
}
