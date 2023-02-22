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
package com.automatics.rdkb.tests.security;

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandFirewallTest extends AutomaticsTestBase {
	
	
	/**
	 * Test to verify WAN - LAN traffic is blocked when Firewall is configured to
	 * Custom Security
	 * 
	 * <ol>
	 * <li>STEP 1: Verify the Ping connection to the gateway from WAN is
	 * successful</li>
	 * <li>STEP 2: Verify whether the firewall setting is configured to 'Custom
	 * Security' for IPv4 traffic</li>
	 * <li>STEP 3: Verify whether the Custom Security is configured to 'Block ICMP'
	 * for IPv4 traffic</li>
	 * <li>STEP 4: Verify the Ping connection to the gateway from WAN should fail
	 * when ICMP requests are blocked under Firewall settings</li>
	 * <li>STEP 5: Verify the Ping connection to the gateway IPv6 Address from WAN
	 * is successful</li>
	 * <li>STEP 6: Verify whether the firewall setting is configured to 'Custom
	 * Security' for IPv6 traffic</li>
	 * <li>STEP 7: Verify whether the Custom Security is configured to 'Block ICMP'
	 * for IPv6 traffic</li>
	 * <li>STEP 8: Verify the Ping connection to the gateway IPv6 Address from WAN
	 * should fail when ICMP requests are blocked under Firewall settings</li>
	 * <li>POST-CONDITION 1: Verify whether the 'Block ICMP' for IPv4 traffic can be
	 * disabled</li>
	 * <li>POST-CONDITION 2: Verify whether the firewall setting is configured to
	 * 'Minimum Security' for IPv4 traffic</li>
	 * <li>POST-CONDITION 3: Verify whether the 'Block ICMP' for IPv6 traffic can be
	 * disabled</li>
	 * <li>POST-CONDITION 4: Verify whether the firewall setting is configured to
	 * 'Typical Security' for IPv6 traffic</li>
	 * </ol>
	 * 
	 * @param device
	 * 
	 * @author Sathya Kishore
	 * @refactor Athira
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-FIREWALL-1000")
	public void testToVerifyWantoLantrafficInCustomFirewall(Dut device) {

		String testId = "TC-RDKB-FIREWALL-100";
		int stepNumber = 1;
		String testStepNumber = "S" + stepNumber;
		String errorMessage = null;
		boolean status = false;
		BroadBandResultObject result = new BroadBandResultObject();
		String wanIpv4 = null;

		try {

			LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-FIREWALL-1000 #####################");
			LOGGER.info(
					"TEST DESCRIPTION: Verify WAN - LAN traffic is blocked when Firewall is configured to Custom Security");

			LOGGER.info("TEST STEPS : ");
			LOGGER.info("1: Verify the firewall setting is configured to 'Custom Security' for IPv4 traffic");
			LOGGER.info("2: Verify the Ping connection to the gateway from WAN is successful");
			LOGGER.info("3: Verify the Custom Security is configured to 'Block ICMP' for IPv4 traffic");
			LOGGER.info(
					"4: Verify the Ping connection to the gateway from WAN should fail when ICMP requests are blocked under Firewall settings");
			LOGGER.info("5: Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic");
			LOGGER.info("6: Verify the Ping connection to the gateway IPv6 Address from WAN is successful");
			LOGGER.info("7: Verify the Custom Security is configured to 'Block ICMP' for IPv6 traffic");
			LOGGER.info(
					"8: Verify the Ping connection to the gateway IPv6 Address from WAN should fail when ICMP requests are blocked under Firewall settings");
			LOGGER.info("POST-CONDITION 1: Verify the 'Block ICMP' for IPv4 traffic can be disabled");
			LOGGER.info(
					"POST-CONDITION 2: Verify the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
			LOGGER.info("POST-CONDITION 3: Verify the 'Block ICMP' for IPv6 traffic can be disabled");
			LOGGER.info(
					"POST-CONDITION 4: Verify the firewall setting is configured to 'Typical Security' for IPv6 traffic");
			LOGGER.info("#####################################################################################");

			/**
			 * Step 1: Verify the firewall setting is configured to 'Custom Security' for
			 * IPv4 traffic
			 */
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify the firewall setting is configured to 'Custom Security' for IPv4 traffic");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'Custom', Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6 ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Firewall Setting for IPv4 traffic should be set to Custom Security");
			LOGGER.info("************************************************************************************");
			
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.FIREWALL_CUSTOM_SECURITY);
			errorMessage = "Firewall Setting for IPv4 traffic cannot be set to Custom Security";
			if (status) {
				LOGGER.info(testStepNumber
						+ " ACTUAL: Firewall Setting for IPv4 traffic is successfully set to Custom Security");
				// wait for 30 seconds to apply settings
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
			}
			LOGGER.info("************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 2: Verify the Ping connection to the gateway from WAN is successful
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify the Ping connection to the gateway IPv4 Address from WAN is successful before blocking ICMP IPv4 Traffic");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute the command from WAN(Jump Server): ping -c 4 <WAN IPv4> ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Ping request to the gateway from WAN should be successful");
			LOGGER.info("************************************************************************************");

		 
			if (DeviceModeHandler.isDSLDevice(device)) {
				LOGGER.info("SKIPPING THIS STEP SINCE MAPT LINE IS PRESENT");
				LOGGER.info("************************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						"NOT APPLICABLE SINCE MAP-T LINE PRESENT", false);

			} else {
				wanIpv4 = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV4);
				LOGGER.info("WAN IPv4 ADDRESS OF THE DEVICE: " + wanIpv4);
				errorMessage = "Unable to retrieve WAN IPv4 Address of the gateway using WebPA/dmcli. ACTUAL RESPONSE: "
						+ wanIpv4;
				if (CommonMethods.isNotNull(wanIpv4) && CommonMethods.isIpv4Address(wanIpv4)) {
					result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, wanIpv4);
					status = result.isStatus();
					errorMessage = result.getErrorMessage();
				}
				if (status) {
					LOGGER.info(testStepNumber
							+ " ACTUAL: Ping request to the gateway IPv4 Address from WAN is successful");
				} else {
					LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
				}
				LOGGER.info("************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}
			/**
			 * Step 3: Verify the Custom Secuirty is configured to 'Block ICMP' for IPv4
			 * traffic
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify the Custom Secuirty is configured to 'Block ICMP' for IPv4 traffic");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'true', Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Firewall Setting for IPv4 traffic should be set to 'Block ICMP' under Custom Security");
			LOGGER.info("************************************************************************************");

			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_ICMP_FOR_IPV4_TRAFFIC_UNDER_CUSTOM_FIREWALL,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
			errorMessage = "Firewall Setting for IPv4 traffic cannot be set to 'Block ICMP' under Custom Security";
			if (status) {
				LOGGER.info(testStepNumber
						+ " ACTUAL: Firewall Setting for IPv4 traffic is successfully set to 'Block ICMP' under Custom Security");
				// wait for 30 seconds to apply settings
				tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
			} else {
				LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
			}
			LOGGER.info("************************************************************************************");
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

			/**
			 * Step 4: Verify the Ping connection to the gateway from WAN should fail when
			 * ICMP requests are blocked under Firewall settings
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify the Ping connection to the gateway IPv4 Address from WAN should fail when ICMP requests are blocked under Firewall settings");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute the command from WAN(Jump Server): ping -c 4 <WAN IPv4> ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Ping request to the gateway from WAN should fail");
			LOGGER.info("************************************************************************************");
			
		  
			if (DeviceModeHandler.isDSLDevice(device)) {
				LOGGER.info("SKIPPING THIS STEP SINCE MAPT LINE IS PRESENT");
				LOGGER.info("************************************************************************************");
				tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
						"NOT APPLICABLE SINCE MAP-T LINE PRESENT", false);

			} else {
				result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, wanIpv4);
				errorMessage = result.isStatus()
						? "Ping to gateway IPv4 Address from WAN is successful even after blocking ICMP Traffic"
						: result.getErrorMessage();
				status = !result.isStatus()
						&& CommonUtils.patternSearchFromTargetString(errorMessage, "Ping from WAN got failed");
				if (status) {
					LOGGER.info(testStepNumber
							+ " ACTUAL: Ping to the gateway IPv4 Address from WAN failed as expected after blocking ICMP Traffic");
				} else {
					LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
				}
				LOGGER.info("************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
			}

			/**
			 * Step 5: Verify the firewall setting is configured to 'Custom Security' for
			 * IPv6 traffic
			 */
			stepNumber++;
			testStepNumber = "S" + stepNumber;
			status = false;
			
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'Custom', Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6 ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Firewall Setting for IPv6 traffic should be set to Custom Security");
			LOGGER.info("************************************************************************************");
			
			if (!(DeviceModeHandler.isFibreDevice(device) || DeviceModeHandler.isDSLDevice(device))) {
				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL_IPV6, WebPaDataTypes.STRING.getValue(),
						BroadBandTestConstants.FIREWALL_CUSTOM_SECURITY);
				errorMessage = "Firewall Setting for IPv6 traffic cannot be set to Custom Security";
				if (status) {
					LOGGER.info(testStepNumber
							+ " ACTUAL: Firewall Setting for IPv6 traffic is successfully set to Custom Security");
					// wait for 30 seconds to apply settings
					tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				} else {
					LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
				}
				LOGGER.info("************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

				/**
				 * Step 6: Verify the Ping connection to the gateway IPv6 Address from WAN is
				 * successful
				 */
				stepNumber++;
				testStepNumber = "S" + stepNumber;
				status = false;
				String wanIpv6 = null;
				if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
					LOGGER.info("************************************************************************************");
					LOGGER.info("STEP " + stepNumber
							+ ": DESCRIPTION : Verify the Ping connection to the gateway IPv6 Address from WAN is successful before blocking ICMP IPv6 Traffic");
					LOGGER.info("STEP " + stepNumber
							+ ": ACTION : Execute the command from WAN(Jump Server): ping -c 4 -W 5 <WAN IPv6> ");
					LOGGER.info("STEP " + stepNumber
							+ ": EXPECTED : Ping request to the gateway from WAN should be successful");
					LOGGER.info("************************************************************************************");

					wanIpv6 = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);

					LOGGER.info("Wan Ipv6 Address is = " + wanIpv6);

					LOGGER.info("WAN IPv6 ADDRESS OF THE DEVICE: " + wanIpv6);
					errorMessage = "Unable to retrieve WAN IPv6 Address of the gateway. ACTUAL RESPONSE: " + wanIpv6;
					if (CommonMethods.isNotNull(wanIpv6) && CommonMethods.isIpv6Address(wanIpv6)) {
						result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, wanIpv6);
						status = result.isStatus();
						errorMessage = result.getErrorMessage();
					}
					if (status) {
						LOGGER.info(testStepNumber
								+ " ACTUAL: Ping request to the gateway IPv6 Address from WAN is successful");
					} else {
						LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
					}
					LOGGER.info("************************************************************************************");
					tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
				} else {
					LOGGER.info("IPv6 is disabled/not available : skipping teststep ...");
					tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
				}

				/**
				 * Step 7: Verify the Custom Secuirty is configured to 'Block ICMP' for IPv6
				 * traffic
				 */
				stepNumber++;
				testStepNumber = "S" + stepNumber;
				status = false;
				LOGGER.info("************************************************************************************");
				LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify the Custom Secuirty is configured to 'Block ICMP' for IPv6 traffic");
				LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'true', Device.X_CISCO_COM_Security.Firewall.FilterAnonymousInternetRequestsV6 ");
				LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Firewall Setting for IPv6 traffic should be set to 'Block ICMP' under Custom Security");
				LOGGER.info("************************************************************************************");

				status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_ICMP_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
				errorMessage = "Firewall Setting for IPv6 traffic cannot be set to 'Block ICMP' under Custom Security";
				if (status) {
					LOGGER.info(testStepNumber
							+ " ACTUAL: Firewall Setting for IPv6 traffic is successfully set to 'Block ICMP' under Custom Security");
					// wait for 30 seconds to apply settings
					tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
				} else {
					LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
				}
				LOGGER.info("************************************************************************************");
				tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

				/**
				 * Step 8: Verify the Ping connection to the gateway IPv6 Address from WAN
				 * should fail when ICMP requests are blocked under Firewall settings
				 */
				stepNumber++;
				testStepNumber = "S" + stepNumber;
				status = false;
				if (BroadbandPropertyFileHandler.isIpv6Enabled()) {
					LOGGER.info("************************************************************************************");
					LOGGER.info("STEP " + stepNumber
							+ ": DESCRIPTION : Verify the Ping connection to the gateway IPv6 Address from WAN should fail when ICMP requests are blocked under Firewall settings");
					LOGGER.info("STEP " + stepNumber
							+ ": ACTION : Execute the command from WAN(Jump Server): ping -c 4 -W 5 <WAN IPv6> ");
					LOGGER.info("STEP " + stepNumber
							+ ": EXPECTED : Ping request to the gateway IPv6 Address from WAN should fail");
					LOGGER.info("************************************************************************************");

					result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, wanIpv6);
					errorMessage = result.isStatus()
							? "Ping to gateway IPv6 Address from WAN is successful even after blocking ICMP Traffic"
							: result.getErrorMessage();
					status = !result.isStatus()
							&& CommonUtils.patternSearchFromTargetString(errorMessage, "Ping from WAN got failed");
					if (status) {
						LOGGER.info(testStepNumber
								+ " ACTUAL: Ping to the gateway IPv6 Address from WAN failed as expected after blocking ICMP Traffic");
					} else {
						LOGGER.error(testStepNumber + " ACTUAL: " + errorMessage);
					}
					LOGGER.info("************************************************************************************");
					tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
				} else {
					LOGGER.info("IPv6 is disabled/not available : skipping teststep ...");
					tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
				}
			} else {

				while (stepNumber <= 8) {
					testStepNumber = "S" + stepNumber;
					LOGGER.info(
							"STEP " + stepNumber + ": TEST STEP NOT APPLICABLE FOR DEVICE MODEL :" + device.getName());
					errorMessage = BroadBandTestConstants.OPERATING_STANDARDS_N + "Not supported for "
							+ device.getModel();
					LOGGER.info("**********************************************************************************");
					tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
							errorMessage, false);
					stepNumber++;
				}

			}

		} catch (Exception testException) {
			errorMessage = testException.getMessage();
			LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING WAN - LAN TRAFFIC IS BLOCKED IN CUSTOM FIREWALL : "
					+ errorMessage);
			tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		} finally {
			LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify whether the 'Block ICMP' for IPv4 traffic can be disabled");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'false', Device.X_CISCO_COM_Security.Firewall.FilterAnonymousInternetRequests ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : 'Block ICMP' under Custom Security should be disabled for IPv4 Traffic");
			LOGGER.info("************************************************************************************");

			if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_ICMP_FOR_IPV4_TRAFFIC_UNDER_CUSTOM_FIREWALL,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
				LOGGER.info(
						"POST-CONDITION-1 PASSED: 'Block ICMP' under Custom Security is disabled successfully for IPv4 Traffic");
			} else {
				LOGGER.error(
						"POST-CONDITION-1 FAILED: 'Block ICMP' under Custom Security cannot be disabled for IPv4 Traffic");
			}
			LOGGER.info("#####################################################################################");
			
			LOGGER.info("************************************************************************************");
			LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
			LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'Low', Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6 ");
			LOGGER.info("STEP "+ stepNumber + ": EXPECTED : Firewall Setting for IPv4 traffic should be set to 'Minimum Security'");
			LOGGER.info("************************************************************************************");
			
			if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.FIREWALL_IPV4_MINIMUM_SECURITY)) {
				LOGGER.info(
						"POST-CONDITION-2 PASSED: Firewall Setting for IPv4 traffic is set successfully to 'Minimum Security'");
			} else {
				LOGGER.error(
						"POST-CONDITION-2 FAILED: Firewall Setting for IPv4 traffic cannot be set to 'Minimum Security'");
			}
			LOGGER.info("#####################################################################################");

			if (!(DeviceModeHandler.isFibreDevice(device))) {
				
				LOGGER.info("************************************************************************************");
				LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify whether the 'Block ICMP' for IPv6 traffic can be disabled");
				LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'false', Device.X_CISCO_COM_Security.Firewall.FilterAnonymousInternetRequestsV6 ");
				LOGGER.info("STEP "+ stepNumber + ": EXPECTED : 'Block ICMP' under Custom Security should be disabled for IPv6 Traffic");
				LOGGER.info("************************************************************************************");
				
				if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_ICMP_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
						WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
					LOGGER.info(
							"POST-CONDITION-3 PASSED: 'Block ICMP' under Custom Security is disabled successfully for IPv6 Traffic");
				} else {
					LOGGER.error(
							"POST-CONDITION-3 FAILED: 'Block ICMP' under Custom Security cannot be disabled for IPv6 Traffic");
				}
				LOGGER.info("#####################################################################################");
				LOGGER.info("************************************************************************************");
				LOGGER.info("STEP "+ stepNumber + ": DESCRIPTION : Verify whether the firewall setting is configured to 'Typical Security' for IPv6 traffic");
				LOGGER.info("STEP "+ stepNumber + ": ACTION : Execute set command for the following webpa params to set value to 'Default', Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6 ");
				LOGGER.info("STEP "+ stepNumber + ": EXPECTED :  Firewall Setting for IPv6 traffic should be set to 'Typical Security'");
				LOGGER.info("************************************************************************************");
				
				LOGGER.info(
						"POST-CONDITION 4: DESCRIPTION: Verify whether the firewall setting is configured to 'Typical Security' for IPv6 traffic");
				LOGGER.info(
						"POST-CONDITION 4: EXPECTED: Firewall Setting for IPv6 traffic should be set to 'Typical Security'");
				if (BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL_IPV6, WebPaDataTypes.STRING.getValue(),
						BroadBandTestConstants.FIREWALL_IPV6_TYPICAL_SECURITY)) {
					LOGGER.info(
							"POST-CONDITION-4 PASSED: Firewall Setting for IPv6 traffic is set successfully to 'Typical Security'");
				} else {
					LOGGER.error(
							"POST-CONDITION-4 FAILED: Firewall Setting for IPv6 traffic cannot be set to 'Typical Security'");
				}
			} else {
				LOGGER.info("POST-CONDITION 3 AND 4 IS NOT APPLICABLE FOR DEVICE MODEL :" + device.getName());
			}
			LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY_FIREWALL-1000");
	}

    /**
     * Verify security firewall paramaters are adding rules in iptables after enabled
     * <ol>
     * <li>Perform factory reset using Webpa</li>
     * <li>Verify ipv4 fragmented security parameter is having default value using webpa</li>
     * <li>Verify ipv4 Flood detect security parameter is having default value using webpa</li>
     * <li>Verify ipv4 portscan security parameter is having default value using webpa</li>
     * <li>Verify ipv6 fragmented security parameter is having default value using webpa</li>
     * <li>Verify ipv6 Flood detect security parameter is having default value using webpa</li>
     * <li>Verify ipv6 portscan security parameter is having default value using webpa</li>
     * <li>Enable ipv4 fragmented security parameter using webpa</li>
     * <li>Enable ipv4 Flood detect security parameter using webpa</li>
     * <li>Enable ipv4 portscan security parameter using webpa</li>
     * <li>Enable ipv6 fragmented security parameter using webpa</li>
     * <li>Enable ipv6 Flood detect security parameter using webpa</li>
     * <li>Enable ipv6 portscan security parameter using webpa</li>
     * <li>Verify iptables rules are added for ipv4 fragmented security</li>
     * <li>Verify iptables rules are added for ipv4 Flood detect security</li>
     * <li>Verify iptables rules are added for ipv4 portscan security</li>
     * <li>Verify iptables rules are added for ipv6 fragmented security</li>
     * <li>Verify iptables rules are added for ipv6 Flood detect security</li>
     * <li>Verify iptables rules are added for ipv6 portscan security</li>
     * <li>Disable ipv4 fragmented security parameter using webpa</li>
     * <li>Disable ipv4 Flood detect security parameter using webpa</li>
     * <li>Disable ipv4 portscan security parameter using webpa</li>
     * <li>Disable ipv6 fragmented security parameter using webpa</li>
     * <li>Disable ipv6 Flood detect security parameter using webpa</li>
     * <li>Disable ipv6 portscan security parameter using webpa</li>
     * <li>Verify iptables rules are removed for ipv4 fragmented security after disabled</li>
     * <li>Verify iptables rules are removed for ipv4 Flood detect security</li>
     * <li>Verify iptables rules are removed for ipv4 portscan security</li>
     * <li>Verify iptables rules are removed for ipv6 fragmented security after disabled</li>
     * <li>Verify iptables rules are removed for ipv6 Flood detect security</li>
     * <li>Verify iptables rules are removed for ipv6 portscan security</li>
     * </ol>
     * 
     * @author Betel Costrow
     * @Refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SECURITY)
    @TestDetails(testUID = "TC-RDKB-SECURITY_FIREWALL-1001")
    public void testToVerifySecurityFirewallParams(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SECURITY_FIREWALL-001";
	String stepNum = "s1";
	String errorMessage = "Not able to perform factory reset.";
	boolean isFactoryReset = false;
	boolean status = false;
	String response = null;
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SECURITY_FIREWALL-1001");
	LOGGER.info("TEST DESCRIPTION: Verify security firewall paramaters are adding rules in iptables after enabled");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Perform factory reset using Webpa");
	LOGGER.info("2. Verify ipv4 fragmented security parameter is having default value using  webpa");
	LOGGER.info("3. Verify ipv4 Flood detect security parameter is having default value using  webpa");
	LOGGER.info("4. Verify ipv4 portscan security parameter is having default value using  webpa");
	LOGGER.info("5. Verify ipv6 fragmented security parameter is having default value using  webpa");
	LOGGER.info("6. Verify ipv6 Flood detect security parameter is having default value using  webpa");
	LOGGER.info("7. Verify ipv6 portscan security parameter is having default value using  webpa");
	LOGGER.info("8. Enable ipv4 fragmented security parameter using webpa");
	LOGGER.info("9. Enable ipv4 Flood detect security parameter using webpa");
	LOGGER.info("10. Enable ipv4 portscan security parameter using webpa");
	LOGGER.info("11. Enable ipv6 fragmented security parameter using webpa");
	LOGGER.info("12. Enable ipv6 Flood detect security parameter using webpa");
	LOGGER.info("13. Enable ipv6 portscan security parameter using webpa");
	LOGGER.info("14. Verify iptables rules are added for ipv4  fragmented security");
	LOGGER.info("15. Verify iptables rules are added for ipv4 Flood detect security ");
	LOGGER.info("16. Verify iptables rules are added for ipv4 portscan security ");
	LOGGER.info("17. Verify iptables rules are added for ipv6  fragmented security");
	LOGGER.info("18. Verify iptables rules are added for ipv6 Flood detect security ");
	LOGGER.info("19. Verify iptables rules are added for ipv6 portscan security ");
	LOGGER.info("20. Disable ipv4 fragmented security parameter using webpa");
	LOGGER.info("21. Disable ipv4 Flood detect security parameter using webpa");
	LOGGER.info("22. Disable ipv4 portscan security parameter using webpa");
	LOGGER.info("23. Disable ipv6 fragmented security parameter using webpa");
	LOGGER.info("24. Disable ipv6 Flood detect security parameter using webpa");
	LOGGER.info("25. Disable ipv6 portscan security parameter using webpa");
	LOGGER.info("26. Verify iptables rules are removed for ipv4 fragmented security after disabled");
	LOGGER.info("27. Verify iptables rules are removed for ipv4 Flood detect security");
	LOGGER.info("28. Verify iptables rules are removed for ipv4 portscan security");
	LOGGER.info("29. Verify iptables rules are removed for ipv6 fragmented security after disabled");
	LOGGER.info("30. Verify iptables rules are removed for ipv6 Flood detect security");
	LOGGER.info("31. Verify iptables rules are removed for ipv6 portscan security");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Perform factory reset using Webpa");
	    LOGGER.info(
		    "STEP 1: ACTION : Exeucte webpa set command:parameter: Device.X_CISCO_COM_DeviceControl.FactoryReset Datatype: string value: \"Router,Wifi,VoIP,Dect,MoCA\"");
	    LOGGER.info("STEP 1: EXPECTED : Device should go for factory reset");
	    LOGGER.info("***************************************************************************************");

	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

	    if (status) {
		isFactoryReset = status;
		LOGGER.info("STEP 1: ACTUAL : Performed factory reset successfully");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s2";
	    errorMessage = "Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts is responding true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify ipv4 fragmented security parameter is having default value using  webpa");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute get command:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts should respond false ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_FRAGMENTATION_IPPKTS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Successfully verified ipv4 fragmented security parameter is having default value");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect is responding true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify ipv4 Flood detect security parameter is having default value using  webpa");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute get command:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect should respond false ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_IPFLOOD_DETECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Successfully verified ipv4 Flood detect security parameter is having default value");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect is responding true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify ipv4 portscan security parameter is having default value using  webpa");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute get command:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect should respond false ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_PORTSCAN_PROTECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Successfully verified ipv4 portscan security parameter is having default value");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts is rsponding true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify ipv6 fragmented security parameter is having default value using  webpa");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute get command:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts");
	    LOGGER.info(
		    "STEP 5: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts should respond false ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_FRAGMENTATION_IPPKTS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Successfully verified ipv6 fragmented security parameter is having default value");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect is responding true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify ipv6 Flood detect security parameter is having default value using  webpa");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute get command:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect should respond false ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_IPFLOOD_DETECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Successfully verified ipv6 Flood detect security parameter is having default value");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect is responding true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify ipv6 portscan security parameter is having default value using  webpa");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute get command:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect should respond false ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_PORTSCAN_PROTECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Successfully verified ipv6 portscan security parameter is having default value");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Not able to enable Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Enable ipv4 fragmented security parameter using webpa");
	    LOGGER.info("STEP 8: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts");
	    LOGGER.info(
		    "STEP 8: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_FRAGMENTATION_IPPKTS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully enabled ipv4 fragmented security parameter using webpa");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Not able to enable Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Enable ipv4 Flood detect security parameter using webpa");
	    LOGGER.info("STEP 9: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect");
	    LOGGER.info(
		    "STEP 9: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_IPFLOOD_DETECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully enabled ipv4 Flood detect security parameter using webpa");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Not able to enable Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Enable ipv4 portscan security parameter using webpa");
	    LOGGER.info("STEP 10: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_PORTSCAN_PROTECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfully enabled ipv4 portscan security parameter using webpa");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Not able to enable Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Enable ipv6 fragmented security parameter using webpa");
	    LOGGER.info("STEP 11: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts");
	    LOGGER.info(
		    "STEP 11: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_FRAGMENTATION_IPPKTS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully enabled ipv6 fragmented security parameter using webpa");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Not able to enable Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Enable ipv6 Flood detect security parameter using webpa");
	    LOGGER.info("STEP 12: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect");
	    LOGGER.info(
		    "STEP 12: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_IPFLOOD_DETECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Successfully enabled ipv6 Flood detect security parameter using webpa");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Not able to enable Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Enable ipv6 portscan security parameter using webpa");
	    LOGGER.info("STEP 13: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect");
	    LOGGER.info(
		    "STEP 13: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_PORTSCAN_PROTECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Successfully enabled ipv6 portscan security parameter using webpa");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s14";
	    errorMessage = "Rules are not updated in iptables after enabled fragmented security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 14: DESCRIPTION : Verify iptables rules are added for ipv4  fragmented security");
	    LOGGER.info("STEP 14: ACTION : Execute:iptables -S | grep FRAG_DROP");
	    LOGGER.info(
		    "STEP 14: EXPECTED : Response should have all the rules applicable for ipv4 fragmented security");
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("Waiting for 30 seconds to iptables get update");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE
		    + BroadBandTraceConstants.LOG_MESSAGE_FRAG_DROP);
	    if (CommonMethods.isNotNull(response)) {
		broadBandResultObject = BroadBandCommonUtils.isGivenStringListAvailableInCommandOutput(response,
			BroadBandTestConstants.LIST_IPV_4_FRAG_IP_PKTS_RULES);
		status = broadBandResultObject.isStatus();
	    }

	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : Ipv4  fragmented security rules are added to iptables after enabled");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s15";
	    errorMessage = "Rules are not updated in iptables after enabled Flood detect security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 15: DESCRIPTION : Verify iptables rules are added for ipv4 Flood detect security ");
	    LOGGER.info("STEP 15: ACTION : Execute:iptables -S | grep DOS");
	    LOGGER.info(
		    "STEP 15: EXPECTED : Response should have all the rules applicable for ipv4 Flood detect security ");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE
		    + BroadBandTraceConstants.LOG_MESSAGE_DOS);
	    if (CommonMethods.isNotNull(response)) {
		broadBandResultObject = BroadBandCommonUtils.isGivenStringListAvailableInCommandOutput(response,
			BroadBandTestConstants.LIST_IPV_4_6_FLOOD_DETECT_RULES);
		status = broadBandResultObject.isStatus();
	    }

	    if (status) {
		LOGGER.info("STEP 15: ACTUAL : Ipv4 Flood detect security rules are added to iptables after enabled");
	    } else {
		LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s16";
	    errorMessage = "Rules are not updated in iptables after enabled portscan security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 16: DESCRIPTION : Verify iptables rules are added for ipv4 portscan security ");
	    LOGGER.info("STEP 16: ACTION : Execute:iptables -S | grep PORT_SCAN");
	    LOGGER.info(
		    "STEP 16: EXPECTED : Response should have all the rules applicable for ipv4 portscan security ");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE
		    + BroadBandTraceConstants.LOG_MESSAGE_PORT_SCAN);
	    if (CommonMethods.isNotNull(response)) {
		broadBandResultObject = BroadBandCommonUtils.isGivenStringListAvailableInCommandOutput(response,
			BroadBandTestConstants.LIST_IPV_4_PORT_SCAN_RULES);
		status = broadBandResultObject.isStatus();
	    }

	    if (status) {
		LOGGER.info("STEP 16: ACTUAL : Ipv4 portscan security rules are added to iptables after enabled");
	    } else {
		LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s17";
	    errorMessage = "Rules are not updated in iptables after enabled fragmented security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 17: DESCRIPTION : Verify iptables rules are added for ipv6  fragmented security");
	    LOGGER.info("STEP 17: ACTION : Execute:ip6tables -S | grep FRAG_DROP");
	    LOGGER.info(
		    "STEP 17: EXPECTED : Response should have all the rules applicable for ipv6 fragmented security");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES
		    + BroadBandTraceConstants.LOG_MESSAGE_FRAG_DROP);
	    if (CommonMethods.isNotNull(response)) {
		broadBandResultObject = BroadBandCommonUtils.isGivenStringListAvailableInCommandOutput(response,
			BroadBandTestConstants.LIST_IPV_6_FRAG_IP_PKTS_RULES);
		status = broadBandResultObject.isStatus();
	    }

	    if (status) {
		LOGGER.info("STEP 17: ACTUAL : Ipv6 fragmented security rules are added to iptables after enabled");
	    } else {
		LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s18";
	    errorMessage = "Rules are not updated in iptables after enabled Flood detect security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION : Verify iptables rules are added for ipv6 Flood detect security ");
	    LOGGER.info("STEP 18: ACTION : Execute:ip6tables -S | grep DOS");
	    LOGGER.info(
		    "STEP 18: EXPECTED : Response should have all the rules applicable for ipv6 Flood detect security ");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES
		    + BroadBandTraceConstants.LOG_MESSAGE_DOS);
	    if (CommonMethods.isNotNull(response)) {
		broadBandResultObject = BroadBandCommonUtils.isGivenStringListAvailableInCommandOutput(response,
			BroadBandTestConstants.LIST_IPV_4_6_FLOOD_DETECT_RULES);
		status = broadBandResultObject.isStatus();
	    }

	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Ipv6 Flood detect security rules are added to iptables after enabled");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s19";
	    errorMessage = "Rules are not updated in iptables after enabled portscan security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION : Verify iptables rules are added for ipv6 portscan security ");
	    LOGGER.info("STEP 19: ACTION : Execute:ip6tables -S | grep PORT_SCAN");
	    LOGGER.info(
		    "STEP 19: EXPECTED : Response should have all the rules applicable for ipv6 portscan security ");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES
		    + BroadBandTraceConstants.LOG_MESSAGE_PORT_SCAN);
	    if (CommonMethods.isNotNull(response)) {
		broadBandResultObject = BroadBandCommonUtils.isGivenStringListAvailableInCommandOutput(response,
			BroadBandTestConstants.LIST_IPV_6_PORT_SCAN_RULES);
		status = broadBandResultObject.isStatus();
	    }

	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Ipv6 portscan security rules are added to iptables after enabled");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s20";
	    errorMessage = "Not able to disable Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION : Disable ipv4 fragmented security parameter using webpa");
	    LOGGER.info("STEP 20: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts");
	    LOGGER.info(
		    "STEP 20: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.BlockFragIPPkts should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_FRAGMENTATION_IPPKTS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 20: ACTUAL : Successfully disabled ipv4 fragmented security parameter using webpa");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s21";
	    errorMessage = "Not able to disable Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 21: DESCRIPTION : Disable ipv4 Flood detect security parameter using webpa");
	    LOGGER.info("STEP 21: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect");
	    LOGGER.info(
		    "STEP 21: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.IPFloodDetect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_IPFLOOD_DETECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 21: ACTUAL : Successfully disabled ipv4 Flood detect security parameter using webpa");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s22";
	    errorMessage = "Not able to Disable Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 22: DESCRIPTION : Disable ipv4 portscan security parameter using webpa");
	    LOGGER.info("STEP 22: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect");
	    LOGGER.info(
		    "STEP 22: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V4.PortScanProtect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_PORTSCAN_PROTECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_4),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 22: ACTUAL : Successfully disabled ipv4 portscan security parameter using webpa");
	    } else {
		LOGGER.error("STEP 22: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s23";
	    errorMessage = "Not able to disable Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 23: DESCRIPTION : Disable ipv6 fragmented security parameter using webpa");
	    LOGGER.info("STEP 23: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts");
	    LOGGER.info(
		    "STEP 23: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.BlockFragIPPkts should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_FRAGMENTATION_IPPKTS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 23: ACTUAL : Successfully disabled ipv6 fragmented security parameter using webpa");
	    } else {
		LOGGER.error("STEP 23: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s24";
	    errorMessage = "Not able to disable Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 24: DESCRIPTION : Disable ipv6 Flood detect security parameter using webpa");
	    LOGGER.info("STEP 24: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect");
	    LOGGER.info(
		    "STEP 24: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.IPFloodDetect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_IPFLOOD_DETECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 24: ACTUAL : Successfully disabled ipv6 Flood detect security parameter using webpa");
	    } else {
		LOGGER.error("STEP 24: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s25";
	    errorMessage = "Not able to Disable Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 25: DESCRIPTION : Disable ipv6 portscan security parameter using webpa");
	    LOGGER.info("STEP 25: ACTION : Execute:Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect");
	    LOGGER.info(
		    "STEP 25: EXPECTED : Device.Firewall.X_RDKCENTRAL-COM_Security.V6.PortScanProtect should response 200 ok msg");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_PORTSCAN_PROTECT
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_6),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 25: ACTUAL : Successfully disabled ipv6 portscan security parameter using webpa");
	    } else {
		LOGGER.error("STEP 25: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s26";
	    errorMessage = "Rules are not removed from iptables after disabled fragmented security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 26: DESCRIPTION : Verify iptables rules are removed for ipv4 fragmented security after disabled");
	    LOGGER.info("STEP 26: ACTION : Execute:iptables -S | grep FRAG_DROP");
	    LOGGER.info(
		    "STEP 26: EXPECTED : Response shouldn't have all the rules applicable for ipv4 fragmented security or response should be null");
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("Waiting for 30 seconds to iptables get update");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE
			    + BroadBandTraceConstants.LOG_MESSAGE_FRAG_DROP));

	    if (status) {
		LOGGER.info(
			"STEP 26: ACTUAL : Ipv4  fragmented security rules are removed from iptables after disabled");
	    } else {
		LOGGER.error("STEP 26: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s27";
	    errorMessage = "Rules are not removed from iptables after disabled Flood detect security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 27: DESCRIPTION : Verify iptables rules are removed for ipv4 Flood detect security");
	    LOGGER.info("STEP 27: ACTION : Execute:iptables -S | grep DOS");
	    LOGGER.info(
		    "STEP 27: EXPECTED : Response shouldn't have all the rules applicable for ipv4 Flood detect security or response should be null");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE
			    + BroadBandTraceConstants.LOG_MESSAGE_DOS));

	    if (status) {
		LOGGER.info(
			"STEP 27: ACTUAL : Ipv4 Flood detect security rules are removed from iptables after disabled");
	    } else {
		LOGGER.error("STEP 27: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s28";
	    errorMessage = "Rules are not removed from iptables after disabled portscan security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 28: DESCRIPTION : Verify iptables rules are removed for ipv4 portscan security");
	    LOGGER.info("STEP 28: ACTION : Execute:iptables -S | grep PORT_SCAN");
	    LOGGER.info(
		    "STEP 28: EXPECTED : Response shouldn't have all the rules applicable for ipv4 portscan security or response should be null");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE
			    + BroadBandTraceConstants.LOG_MESSAGE_PORT_SCAN));

	    if (status) {
		LOGGER.info("STEP 28: ACTUAL : Ipv4 portscan security rules are removed from iptables after disabled");
	    } else {
		LOGGER.error("STEP 28: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s29";
	    errorMessage = "Rules are not updated in iptables after enabled fragmented security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 29: DESCRIPTION : Verify iptables rules are removed for ipv6 fragmented security after disabled");
	    LOGGER.info("STEP 29: ACTION : Execute:ip6tables -S | grep FRAG_DROP");
	    LOGGER.info(
		    "STEP 29: EXPECTED : Response shouldn't have all the rules applicable for ipv6 fragmented security or response should be null");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES
			    + BroadBandTraceConstants.LOG_MESSAGE_FRAG_DROP));

	    if (status) {
		LOGGER.info(
			"STEP 29: ACTUAL : Ipv6 fragmented security rules are removed from iptables after disabled");
	    } else {
		LOGGER.error("STEP 29: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s30";
	    errorMessage = "Rules are not updated in iptables after enabled Flood detect security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 30: DESCRIPTION : Verify iptables rules are removed for ipv6 Flood detect security");
	    LOGGER.info("STEP 30: ACTION : Execute:ip6tables -S | grep DOS");
	    LOGGER.info(
		    "STEP 30: EXPECTED : Response shouldn't have all the rules applicable for ipv6 Flood detect security or response should be null");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES
			    + BroadBandTraceConstants.LOG_MESSAGE_DOS));

	    if (status) {
		LOGGER.info(
			"STEP 30: ACTUAL : Ipv6 Flood detect security rules are removed from iptables after disabled");
	    } else {
		LOGGER.error("STEP 30: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s31";
	    errorMessage = "Rules are not updated in iptables after enabled portscan security parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 31: DESCRIPTION : Verify iptables rules are removed for ipv6 portscan security");
	    LOGGER.info("STEP 31: ACTION : Execute:ip6tables -S | grep PORT_SCAN");
	    LOGGER.info(
		    "STEP 31: EXPECTED : Response shouldn't have all the rules applicable for ipv6 portscan security or response should be null");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(
		    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES
			    + BroadBandTraceConstants.LOG_MESSAGE_PORT_SCAN));

	    if (status) {
		LOGGER.info("STEP 31: ACTUAL : Ipv6 portscan security rules are removed from iptables after disabled");
	    } else {
		LOGGER.error("STEP 31: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    if (isFactoryReset) {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		LOGGER.info("POST-CONDITION STEPS");
		LOGGER.info("POST-CONDITION 1: DESCRIPTION : Begin Broadband device reactivation.");
		LOGGER.info("POST-CONDITION 1: ACTION : Broadband device reactivation.");
		LOGGER.info("POST-CONDITION 1: EXPECTED : device should get reactivated");

		BroadBandWiFiUtils.reactivateBroadBandDeviceWebPa(tapEnv, device);

		LOGGER.info("POST-CONDITION 1: ACTUAL : Reactivated device successfully");
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SECURITY_FIREWALL-1001");
    }
    
	/**
	 * Verify Echo Reply or Response with different firewall settings
	 * <ol>
	 * <li>Verify IPv6 address of google.com is retrieved via nslookup.</li>
	 * <li>Verify the firewall setting is configured to 'Custom Security' for
	 * IPv6 traffic.</li>
	 * <li>Verify whether the 'Block ICMP' for IPv6 traffic can be enabled.</li>
	 * <li>Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of
	 * 'google.com' from WAN is successful with 'Block ICMP' enabled for IPv6
	 * traffic.</li>
	 * <li>Verify whether the 'Block ICMP' for IPv6 traffic can be disabled.
	 * </li>
	 * <li>Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of
	 * 'google.com' from WAN is successful with 'Block ICMP' disabled for IPv6
	 * traffic.</li>
	 * <li>Verify whether the firewall setting is configured to 'Typical
	 * Security' for IPv6 traffic.</li>
	 * <li>Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of
	 * 'google.com' from WAN is successful with firewall 'Typical Security'.
	 * </li>
	 * <li>Verify the firewall setting is configured to 'Maximum Security' for
	 * IPv4 traffic.</li>
	 * <li>Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of
	 * 'google.com' from WAN is successful with firewall 'High'.</li>
	 * <li>Verify whether the firewall setting is configured to 'Minimum
	 * Security' for IPv4 traffic.</li>
	 * <li>Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of
	 * 'google.com' from WAN is successful with firewall 'Low'.</li>
	 * </ol>
	 * 
	 * @param device
	 *            {@link Dut}
	 * 
	 * @author Prashant Mishra
	 * @Refactor Athira
	 * 
	 */
	 
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-FIREWALL-1005")
	public void parentalControlEthernetClient(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-FIREWALL-105";
		String stepNum = "";
		String errorMessage = "";
		String nslookupIPv6Addrfacebook = "";
		boolean status = false;
		BroadBandResultObject result = null;
		List<String> ipAddress = null;
		// Variable Declaration Ends
		
		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-FIREWALL-1005");
		LOGGER.info("TEST DESCRIPTION: Verify Echo Reply or Response with different firewall settings");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify IPv6 address of google.com is retrieved via nslookup.");
		LOGGER.info("2. Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic.");
		LOGGER.info("3. Verify whether the 'Block ICMP' for IPv6 traffic can be enabled.");
		LOGGER.info(
				"4. Verify the ICMPv6-Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with 'Block ICMP' enabled for IPv6 traffic.");
		LOGGER.info("5. Verify whether the 'Block ICMP' for IPv6 traffic can be disabled");
		LOGGER.info(
				"6. Verify the ICMPv6-Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with 'Block ICMP' disabled for IPv6 traffic.");
		LOGGER.info("7. Verify whether the firewall setting is configured to 'Typical Security' for IPv6 traffic.");
		LOGGER.info(
				"8. Verify the ICMPv6-Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with firewall 'Typical Security'.");
		LOGGER.info("9. Verify the firewall setting is configured to 'Maximum Security' for IPv4 traffic.");
		LOGGER.info(
				"10. Verify the ICMPv6-Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with firewall 'High'.");
		LOGGER.info("11. Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
		LOGGER.info(
				"12. Verify the ICMPv6-Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with firewall 'Low'");
		LOGGER.info("#######################################################################################");
		try {
			stepNum = "S1";
			errorMessage = "Unable to get IPv6 address of facebook.com via nslookup.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify IPv6 address of facebook.com is retrieved via nslookup.");
			LOGGER.info(
					"STEP 1: ACTION : SSH the device and Execute the following command: nslookup -query=AAAA facebook.com");
			LOGGER.info("STEP 1: EXPECTED : IPv6 address for facebook.com should be retrieved successfully and saved.");
			LOGGER.info("**********************************************************************************");
			String nslookupResponse = tapEnv.executeCommandUsingSsh(device,
					BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_NSLOOKUP_WITH_PATH,
							BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
							BroadBandTestConstants.NSLOOKUP_FOR_FACEBOOK));
			
			if (CommonMethods.isNotNull(nslookupResponse)) {
				ipAddress = BroadBandCommonUtils.patternFinderForMultipleMatches(nslookupResponse,
						BroadBandTestConstants.PATTERN_TO_RETRIVE_IPV6_ADDRESS_FROM_NSLOOKUP_FACEBOOK_VIA_SSH,
						BroadBandTestConstants.CONSTANT_1);
				if (!ipAddress.isEmpty()) {
					nslookupIPv6Addrfacebook = ipAddress.get(BroadBandTestConstants.CONSTANT_0);
					status = CommonMethods.isNotNull(nslookupIPv6Addrfacebook)
							&& CommonMethods.isIpv6Address(nslookupIPv6Addrfacebook);
					LOGGER.info("IPv6 address of facebook.com: " + nslookupIPv6Addrfacebook);
				} else {
					errorMessage = "Unable to get Ipv6 address via nslookup response.";
				}
			} else {
				errorMessage = "Nslookup response is null.";
			}
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : IPv6 address of facebook.com is retrieved successfully.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S2";
			errorMessage = "Unable to set Firewall Setting to 'Custom Security'.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Verify the firewall setting is configured to 'Custom Security' for IPv6 traffic.");
			LOGGER.info(
					"STEP 2: ACTION : Execute the following webpa params: Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6and set value to 'Custom'.");
			LOGGER.info("STEP 2: EXPECTED : Firewall Setting for IPv6 traffic should be set to 'Custom Security'.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL_IPV6, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.FIREWALL_CUSTOM_SECURITY);
			if (status) {
				LOGGER.info(
						"STEP 2: ACTUAL :  Firewall Setting for IPv6 traffic is successfully set to Custom Security");
			} else {
				LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S3";
			errorMessage = "Firewall Setting for IPv6 traffic cannot be set to 'Block ICMP' enabled under Custom Security.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 3: DESCRIPTION : Verify whether the 'Block ICMP' for IPv6 traffic can be enabled.");
			LOGGER.info(
					"STEP 3: ACTION : Execute the following webpa params: Device.X_CISCO_COM_Security.Firewall.FilterAnonymousInternetRequestsV6 and set value to 'true'.");
			LOGGER.info(
					"STEP 3: EXPECTED : Firewall Setting for IPv6 traffic should be set to 'Block ICMP' enabled under Custom Security.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_ICMP_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
			if (status) {
				LOGGER.info(
						"STEP 3: ACTUAL : Firewall Setting for IPv6 traffic is successfully set with 'Block ICMP' enabled under Custom Security.");
			} else {
				LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S4";
			errorMessage = "Unable to verify ICMPv6- Echo Reply or Response to the IPv6 Address of 'facebook.com' with Firewall setting to 'Custom' and blocked ICMP enabled.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 4: DESCRIPTION : Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with 'Block ICMP' enabled for IPv6 traffic.");
			LOGGER.info(
					"STEP 4: ACTION : SSH the device and execute the following command: ping6 -c 5 <Ipv6 address of facebook.com>");
			LOGGER.info(
					"STEP 4: EXPECTED : Echo Reply or Response to the IPv6 Address of 'facebook.com' should be successful.");
			LOGGER.info("**********************************************************************************");
			result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, nslookupIPv6Addrfacebook);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"STEP 4: ACTUAL : Ping request to the facebook.com IPv6 Address from WAN with Block ICMP enabled for IPv6 traffice is successful.");
			} else {
				LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S5";
			errorMessage = "Failed  to disable 'Block ICMP' for IPv6 traffic. ";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 5: DESCRIPTION : Verify whether the 'Block ICMP' for IPv6 traffic can be disabled.");
			LOGGER.info(
					"STEP 5: ACTION : Execute the following webpa params: Device.X_CISCO_COM_Security.Firewall.FilterAnonymousInternetRequestsV6 and set value to 'false'.");
			LOGGER.info("STEP 5: EXPECTED : 'Block ICMP' under Custom Security should be disabled for IPv6 Traffic.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_TO_BLOCK_ICMP_FOR_IPV6_TRAFFIC_UNDER_CUSTOM_FIREWALL,
					WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
			if (status) {
				LOGGER.info(
						"STEP 5: ACTUAL : Firewall Setting for IPv6 traffic is successfully set with 'Block ICMP' disabled under Custom Security.");
			} else {
				LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S6";
			errorMessage = "Unable to verify ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' with Firewall setting to 'Custom' and blocked ICMP disabled.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with 'Block ICMP' disabled for IPv6 traffic.");
			LOGGER.info(
					"STEP 6: ACTION : SSH the device and execute the following command: ping6 -c 5 <Ipv6 address of google.com>");
			LOGGER.info(
					"STEP 6: EXPECTED : Echo Reply or Response to the IPv6 Address of 'google.com' should be successful.");
			LOGGER.info("**********************************************************************************");
			result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, nslookupIPv6Addrfacebook);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"STEP 6: ACTUAL : Ping request to the facebook.com IPv6 Address from WAN with Block ICMP disabled for IPv6 traffice is successful.");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S7";
			errorMessage = "Unable to set Firewall Setting to 'Typical Security'.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 7: DESCRIPTION : Verify whether the firewall setting is configured to 'Typical Security' for IPv6 traffic.");
			LOGGER.info(
					"STEP 7: ACTION : Execute the following webpa params: Device.X_CISCO_COM_Security.Firewall.FirewallLevelV6 and set value to 'Default'.");
			LOGGER.info("STEP 7: EXPECTED : Firewall Setting for IPv6 traffic should be set to 'Typical Security'.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL_IPV6, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.FIREWALL_IPV6_TYPICAL_SECURITY);
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : Firewall Setting for IPv6 traffic is set successfully to 'Typical Security'");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S8";
			errorMessage = "Unable to verify ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' with Firewall setting to 'Typical Security'.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 8: DESCRIPTION : Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with firewall 'Typical Security'.");
			LOGGER.info(
					"STEP 8: ACTION : SSH the device and execute the following command: ping6 -c 5 <Ipv6 address of google.com>");
			LOGGER.info(
					"STEP 8: EXPECTED : Echo Reply or Response to the IPv6 Address of 'google.com' should be successful.");
			LOGGER.info("**********************************************************************************");
			result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, nslookupIPv6Addrfacebook);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"STEP 8: ACTUAL : Ping request to the facebook.com IPv6 Address from WAN with 'Typical Security' firewall settings for IPv6 traffic is successful.");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
			stepNum = "S9";
			errorMessage = "Unable to set Firewall Setting to 'High'.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 9: DESCRIPTION : Verify the firewall setting is configured to 'Maximum Security' for IPv4 traffic.");
			LOGGER.info(
					"STEP 9: ACTION : Execute the following webpa params: Device.X_CISCO_COM_Security.Firewall.FirewallLevel and set value to 'High'.");
			LOGGER.info("STEP 9: EXPECTED : Firewall Setting for IPv4 traffic should be set to Maximum Security.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.FIREWALL_IPV4_MAXIMUM_SECURITY);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Firewall Setting for IPv4 traffic is successfully set to 'Maximum Security'.");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S10";
			errorMessage = "Unable to verify ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' with Firewall setting to 'High'.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with firewall 'High'.");
			LOGGER.info(
					"STEP 10: ACTION : SSH the device and execute the following command: ping6 -c 5 <Ipv6 address of google.com>");
			LOGGER.info(
					"STEP 10: EXPECTED : Echo Reply or Response to the IPv6 Address of 'google.com' should be successful.");
			LOGGER.info("**********************************************************************************");
			result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, nslookupIPv6Addrfacebook);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : Ping request to the facebook.com IPv6 Address from WAN with 'High' firewall settings is successful.");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S11";
			errorMessage = "Unable to set Firewall Setting to 'Low'";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 11: DESCRIPTION : Verify whether the firewall setting is configured to 'Minimum Security' for IPv4 traffic");
			LOGGER.info(
					"STEP 11: ACTION : Execute the following webpa params: Device.X_CISCO_COM_Security.Firewall.FirewallLevel and set value to 'Low'.");
			LOGGER.info("STEP 11: EXPECTED : Firewall Setting for IPv4 traffic should be set to 'Minimum Security'.");
			LOGGER.info("**********************************************************************************");
			status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
					BroadBandWebPaConstants.WEBPA_PARAM_FIREWALL_LEVEL, WebPaDataTypes.STRING.getValue(),
					BroadBandTestConstants.FIREWALL_IPV4_MINIMUM_SECURITY);
			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : Firewall Setting for IPv4 traffic is successfully set to 'Minimum Security'.");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "S12";
			errorMessage = "Unable to verify ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' with Firewall setting to 'Low'.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Verify the ICMPv6- Echo Reply or Response to the IPv6 Address of 'google.com' from WAN is successful with firewall 'Low'.");
			LOGGER.info(
					"STEP 12: ACTION : SSH the device and execute the following command: ping6 -c 5 <Ipv6 address of google.com>");
			LOGGER.info(
					"STEP 12: EXPECTED : Echo Reply or Response to the IPv6 Address of 'google.com' should be successful.");
			LOGGER.info("**********************************************************************************");
			result = BroadBandCommonUtils.verifyPingConnectionFromJumpServer(device, tapEnv, nslookupIPv6Addrfacebook);
			status = result.isStatus();
			errorMessage = result.getErrorMessage();
			if (status) {
				LOGGER.info(
						"STEP 12: ACTUAL : Ping request to the facebook.com IPv6 Address from WAN with 'Low' firewall settings is successful.");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
			
		}catch (Exception e) {
			LOGGER.error("Exception occured.");
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-FIREWALL-1005");
	}

}
